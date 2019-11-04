use bytes::BytesMut;
use failure::{Error, SyncFailure};
use futures::prelude::*;
use futures::sync::mpsc;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::io::{Error as io_Error, ErrorKind as io_ErrorKind};
use std::net::SocketAddr;
use std::rc::Rc;
use std::str;
use telegram_bot::*;
use tokio_core::net::{TcpListener, TcpStream};
use tokio_core::reactor::{Core, Handle};
use tokio_io::io::{self, ReadHalf};
use tokio_io::AsyncRead;

/// A running Televery server.
pub struct Server {
    /// Tokio Core for running Telegram bots and the TCP listener.
    core: Core,

    /// Telegram API interface.
    api: Option<Api>,

    /// Listener for verification requests.
    listener: Option<TcpListener>,

    /// Trusted apps.
    trusted_apps: Rc<BTreeSet<String>>,

    /// Trusted Telegram usernames.
    trusted_users: Rc<BTreeSet<String>>,

    /// Server state.
    state: Rc<State>,
}

/// Server states.
struct State {
    /// Mapping from usernames to chat ID.
    user_chatid: RefCell<BTreeMap<String, ChatId>>,

    /// Mapping from message ID to socket writer.
    msgid_chan: RefCell<BTreeMap<MessageId, mpsc::UnboundedSender<&'static str>>>,
}

impl Server {
    pub fn new(
        trusted_apps: BTreeSet<String>,
        trusted_users: BTreeSet<String>,
    ) -> Result<Server, Error> {
        let core = Core::new()?;

        Ok(Server {
            core,
            api: None,
            listener: None,
            trusted_apps: Rc::new(trusted_apps),
            trusted_users: Rc::new(trusted_users),
            state: Rc::new(State {
                user_chatid: RefCell::new(BTreeMap::new()),
                msgid_chan: RefCell::new(BTreeMap::new()),
            }),
        })
    }

    /// Bind a local address to listen for verification requests, and
    /// configure Telegram bot API.
    pub fn bind(&mut self, token: impl AsRef<str>, addr: SocketAddr) -> Result<(), Error> {
        let core = &self.core;
        let api = Some(
            Api::configure(token)
                .build(core.handle())
                .map_err(SyncFailure::new)?,
        );
        let listener = Some(TcpListener::bind(&addr, &core.handle())?);

        self.api = api;
        self.listener = listener;
        Ok(())
    }

    /// Run the server and consume the data structure. This function
    /// will make the server listen for incoming verification requests
    /// and Telegram updates. It panics if not bound or binding
    /// failed.
    pub fn run(mut self) -> ! {
        let handle = self.core.handle();
        let api = &self.api.unwrap();
        let listener = self.listener.unwrap();
        let trusted_users = self.trusted_users;
        let trusted_apps = self.trusted_apps;
        let state = self.state;

        // channel between sock and bot sides
        let (sock_tx, bot_rx) = mpsc::unbounded();

        // bot side: communicate with telegram api and socket
        let bot_updates = Server::bot_updates(api, trusted_users.clone(), state.clone());
        let bot_controller = Server::bot_controller(
            api,
            handle.clone(),
            bot_rx,
            trusted_users,
            trusted_apps,
            state,
        );
        let bot = bot_updates.select(bot_controller).map(unit).map_err(unit);

        // socket side:
        let req = Server::local_requests(handle.clone(), listener, sock_tx);

        // join the two sides and run on the default executor
        self.core.run(req.join(bot)).map(unit).unwrap();
        panic!("Unexpected exit from event loop")
    }

    /// This function constructs a future that deals with local
    /// requests.
    fn local_requests(
        handle: Handle,
        listener: TcpListener,
        sock_tx: mpsc::UnboundedSender<ServerMessage>,
    ) -> impl Future<Item = (), Error = ()> {
        let sock_tx = sock_tx.clone();

        listener
            .incoming()
            .for_each(move |(stream, addr)| {
                debug!("New connection: {}", addr);
                let (reader, writer) = stream.split();
                let (chan_tx, chan_rx) = mpsc::unbounded();

                let socket_reader = process_verification_request(reader, chan_tx, sock_tx.clone());
                let socket_writer = chan_rx
                    .fold(writer, |writer, msg: &str| {
                        io::write_all(writer, msg.as_bytes())
                            .map(|(writer, _)| writer)
                            .map_err(|e| error!("in write all: {:?}", e))
                    })
                    .map(unit);

                handle.spawn(socket_reader.select(socket_writer).map(unit).map_err(unit));
                Ok(())
            })
            .map_err(|e| error!("socket error: {:?}", e))
    }

    /// This function constructs a future that deals with Telegram bot
    /// updates.
    fn bot_updates<'a>(
        api: &'a Api,
        trusted_users: Rc<BTreeSet<String>>,
        state: Rc<State>,
    ) -> impl Future<Item = (), Error = ()> + 'a {
        api.stream()
            .for_each(move |update| {
                process_telegram_update(api, update, trusted_users.clone(), state.clone());
                Ok(())
            })
            .map_err(|e| error!("bot updates error: {:?}", e))
    }

    /// This function constructs a future that listens for server's
    /// internal message, and do Telegram-bot-y things on behalf of
    /// the message sender.
    fn bot_controller<'a>(
        api: &'a Api,
        handle: Handle,
        bot_rx: mpsc::UnboundedReceiver<ServerMessage>,
        trusted_users: Rc<BTreeSet<String>>,
        trusted_apps: Rc<BTreeSet<String>>,
        state: Rc<State>,
    ) -> impl Future<Item = (), Error = ()> + 'a {
        bot_rx.for_each(move |message| {
            match message {
                ServerMessage::SendMessage(sock_writer, appname) => {
                    let trusted_users = trusted_users.clone();

                    if !trusted_apps.contains(&appname) {
                        debug!("{} is denied because it's not trusted", appname);
                        sock_writer
                            .unbounded_send(VerifyResult::Deny.into())
                            .unwrap();
                        return Ok(());
                    }

                    // Send confirm messages to every trusted user
                    for user in &*trusted_users {
                        if let Some(fut) = Server::send_confirm(
                            api,
                            user.clone(),
                            appname.clone(),
                            state.clone(),
                            sock_writer.clone(),
                        ) {
                            handle.spawn(fut);
                        }
                    }
                }
            }
            Ok(())
        })
    }

    /// This function constructs a future that sends a confirm message
    /// to a specific user who has already initiated a conversation
    /// with the bot.
    fn send_confirm(
        api: &Api,
        user: String,
        appname: String,
        state: Rc<State>,
        sock_writer: mpsc::UnboundedSender<&'static str>,
    ) -> Option<impl Future<Item = (), Error = ()> + 'static> {
        let state = state.clone();
        let chatid = match state.user_chatid.borrow().get(&user) {
            Some(chatid) => chatid.clone(),
            None => {
                warn!("don't know chat id of user {}, ignoring", user);
                return None;
            }
        };

        let inline_keyboard = reply_markup!(
            inline_keyboard,
            ["Pass" callback "0,0", "Deny" callback "0,1"]
        );

        let msg = format!(
            "Hi, @{}! {} is asking for verification. Shall it pass?",
            user, appname
        );

        Some(
            api.send(requests::SendMessage::new(chatid, msg).reply_markup(inline_keyboard))
                .and_then(move |msg| {
                    debug!("confirm message id = {}", msg.id);
                    // Save message id and channel writer for future use.
                    state.msgid_chan.borrow_mut().insert(msg.id, sock_writer);
                    Ok(())
                })
                .map_err(move |e| error!("in sending message to {}: {:?}", user, e)),
        )
    }
}

/// Process Telegram updates.
fn process_telegram_update(
    api: &Api,
    update: Update,
    trusted_users: Rc<BTreeSet<String>>,
    state: Rc<State>,
) {
    match update.kind {
        UpdateKind::CallbackQuery(query) => process_telegram_callback(api, query, state.clone()),
        UpdateKind::Message(message) => {
            process_telegram_message(api, &message, trusted_users, state.clone());
        }
        _ => (),
    }
}

fn process_telegram_message(
    api: &Api,
    message: &Message,
    trusted_users: Rc<BTreeSet<String>>,
    state: Rc<State>,
) {
    use self::MessageKind::*;
    if let Text { .. } = message.kind {
        let username = &message.from.username;
        let fut = match username {
            Some(username) => {
                if trusted_users.contains(username) {
                    // store chat id for future use
                    state
                        .user_chatid
                        .borrow_mut()
                        .insert(username.clone(), message.chat.id());
                    message.text_reply(format!(
                        "Hi, @{}! This chat (ID = {}) will be used for notifications later on.",
                        username,
                        message.chat.id()
                    ))
                } else {
                    // unknown username
                    message.text_reply(format!(
                        "Sorry, @{}, but you're not authorized to use this bot.",
                        username
                    ))
                }
            }
            None => message.text_reply("You need a username to use this bot."),
        };
        api.spawn(fut);
    }
}

fn process_telegram_callback(api: &Api, query: CallbackQuery, state: Rc<State>) {
    let username = query.from.username.unwrap();
    let response = if query.data == "0,0" {
        debug!("{:?} passed id {}", username, query.message.id);
        VerifyResult::Allow
    } else {
        debug!("{:?} denied id {}", username, query.message.id);
        VerifyResult::Deny
    };

    match state.msgid_chan.borrow().get(&query.message.id).cloned() {
        Some(tx) => {
            tx.unbounded_send(response.into()).unwrap();
            state.msgid_chan.borrow_mut().remove(&query.message.id);
            api.spawn(query.message.edit_text(
                format!(
                    "@{} has answered this verification request.",
                    username
                )
            ))
        }
        None => {
            error!("invalid message id {}", query.message.id);
        }
    }
}

/// Process verification requests. This function will consume a stream
/// and the corresponding address.
fn process_verification_request(
    stream: ReadHalf<TcpStream>,
    chan_tx: mpsc::UnboundedSender<&'static str>,
    sock_tx: mpsc::UnboundedSender<ServerMessage>,
) -> impl Future<Item = (), Error = ()> {
    Frames::new(stream)
        .for_each(move |request| {
            debug!("{} asks {}", request.appname, request.method);
            match request.method.as_str() {
                "REQ" => {
                    sock_tx
                        .unbounded_send(ServerMessage::SendMessage(
                            chan_tx.clone(),
                            request.appname,
                        ))
                        .unwrap();
                }
                _ => {
                    chan_tx.unbounded_send(VerifyResult::Deny.into()).unwrap();
                }
            }
            Ok(())
        })
        .map_err(|e| error!("in process verify: {:?}", e))
}

/// A custom codec for turning stream of bytes into custom requests. I
/// just can't get LineCodec compiling and working...
///
/// Frames takes the ownership of your TcpStream.
#[derive(Debug)]
struct Frames {
    stream: ReadHalf<TcpStream>,
    rd: BytesMut,
}

/// A verification request.
#[derive(Debug, Clone)]
struct VerifyRequest {
    method: String,
    appname: String,
}

/// Possible verification results.
#[derive(Debug, Clone, Copy)]
enum VerifyResult {
    Allow,
    Deny,
}

/// Message type used for channel between the Telegram side and the
/// socket side.
#[derive(Debug, Clone)]
enum ServerMessage {
    // Send a verification message via Telegram bot to the user.
    SendMessage(mpsc::UnboundedSender<&'static str>, String),
}

impl Frames {
    fn new(stream: ReadHalf<TcpStream>) -> Self {
        Frames {
            stream,
            rd: BytesMut::new(),
        }
    }

    /// Read into the buffer whatever has been read by the system so
    /// far.
    fn read_off(&mut self) -> Poll<(), io_Error> {
        loop {
            self.rd.reserve(1024);
            let n = try_ready!(self.stream.read_buf(&mut self.rd));
            if n == 0 {
                return Ok(Async::Ready(()));
            }
        }
    }
}

impl Stream for Frames {
    type Item = VerifyRequest;
    type Error = io_Error;

    fn poll(&mut self) -> Poll<Option<VerifyRequest>, io_Error> {
        let sock_closed = self.read_off()?.is_ready();
        let pos = self.rd.iter().position(|byte| *byte == b'\n');
        if let Some(pos) = pos {
            let mut line = self.rd.split_to(pos + 1);
            line.split_off(pos);

            let line = str::from_utf8(line.as_ref());
            match line {
                Err(_) => {
                    // if request is invalid, close the connection
                    return Err(io_Error::new(
                        io_ErrorKind::InvalidInput,
                        "request is not utf8",
                    ));
                }
                Ok(line) => {
                    return Ok(Async::Ready(Some(line.parse()?)));
                }
            }
        }

        if sock_closed {
            Ok(Async::Ready(None))
        } else {
            Ok(Async::NotReady)
        }
    }
}

impl str::FromStr for VerifyRequest {
    type Err = io_Error;

    fn from_str(s: &str) -> Result<VerifyRequest, io_Error> {
        let parts: Vec<&str> = s.split(' ').collect();
        // fixme: magic number
        if parts.len() != 2 {
            return Err(io_Error::new(
                io_ErrorKind::InvalidInput,
                "invalid length of request",
            ));
        }

        let (method, appname) = (parts[0], parts[1]);
        Ok(VerifyRequest {
            method: method.to_string(),
            appname: appname.to_string(),
        })
    }
}

impl From<VerifyResult> for &'static str {
    fn from(response: VerifyResult) -> &'static str {
        match response {
            VerifyResult::Allow => "ALLOW\n",
            VerifyResult::Deny => "DENY\n",
        }
    }
}

#[doc(hidden)]
fn unit<T>(_: T) {}

//! # Server
//!
//! The server module is responsible for all bussiness logic.

use std::process::{Command, Stdio};
use futures::future::Either;
use futures::sync::mpsc;
use tokio;
use tokio::prelude::*;
use tokio::codec::Decoder;
use tokio::codec::{BytesCodec, FramedRead, FramedWrite};
use tokio::net::{TcpListener, TcpStream};
use tokio::io;
use tokio_process::CommandExt;
use tokio_process::{Child, ChildStdin, ChildStdout};
use bytes::BytesMut;
use failure::Error;

use handshake::handshake;
use streams::{self, Chunk, DecodeError, EncodeError};

pub fn server(
    listener: TcpListener,
    args: Vec<String>
) -> impl Future<Item = (), Error = ()>
{
    listener.incoming().for_each(move |stream| {
        if let Err(e) = do_pipe(stream, &args) {
            error!("Error happened when starting pipe service for client: {}", e)
        }
        future::ok(())
    }).map_err(|e| error!("Error processing clients: {}", e))
}

fn do_pipe(stream: TcpStream, args: &Vec<String>) -> Result<(), io::Error> {
    debug_assert!(args.len() >= 2);

    let mut child = Command::new(&args[1])
        .args(&args[2..])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn_async()?;

    let childin = child.stdin().take().ok_or(io::Error::new(
        io::ErrorKind::BrokenPipe,
        "Cannot take child stdin"
    ))?;
    let childout = child.stdout().take().ok_or(io::Error::new(
        io::ErrorKind::BrokenPipe,
        "Cannot take child stdout"
    ))?;
 
    let conn = handshake(stream)
        .and_then(|cloneable_stream| Ok(cloneable_stream.into_inner()))
        .and_then(|stream| {
            let codec = streams::StreamingCodec::new(true).framed(stream);
            let (netout, netin) = codec.split();

            // Why channel?  Because we want to handle closes
            // properly.  Actually what we really want is a cloneable
            // `netout`, however, unfortunately, `SplitSink` doesn't
            // implement `Clone`.  Of course, we can go the
            // `Arc<Mutex<T>>` way, but we will have to reimplement a
            // bunch of traits. Yeah, I'm being a little lazy here...
            let (tx, rx) = mpsc::unbounded();

            tokio::spawn(from_ws_to_child(childin, netin, tx.clone()));
            tokio::spawn(from_child_to_tx(childout, tx));
            tokio::spawn(from_tx_to_ws(rx, child, netout));

            future::ok(())
        });

    tokio::spawn(conn);
    Ok(())
}

/// Copy child output to tx.
fn from_child_to_tx(
    childout: ChildStdout,
    tx: mpsc::UnboundedSender<TxMessage>
) -> impl Future<Item = (), Error = ()>
{
    FramedRead::new(childout, BytesCodec::new())
        .fold(tx, |tx, out| {
            tx.send(TxMessage::ChildOutput(out))
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
        })
        .map(|_| debug!("Dropping from_child_to_tx"))
        .map_err(|e| error!("During from_child_to_tx: {}", e))
}

/// Copy Websocket input to child.
///
/// When a Close frame is received, this stream reports an
/// error to interrupt the TCP stream, and hence `childin`
/// is dropped.
fn from_ws_to_child(
    childin: ChildStdin,
    netin: impl Stream<Item = Chunk, Error = DecodeError>,
    tx: mpsc::UnboundedSender<TxMessage>,
) -> impl Future<Item = (), Error = ()>
{
    let mut tx = tx.wait();
    netin
        .map_err(ServerError::DecodeError)
        .fold(FramedWrite::new(childin, BytesCodec::new()),
              move |childin, chunk| {
                  debug!("from ws: {:?}", chunk);
                  match chunk {
                      Chunk::Data(data) => Either::A(
                          childin.send(data.freeze())
                              .map_err(ServerError::IOError)
                      ),
                      Chunk::Close(data) => Either::B({
                          tx.send(TxMessage::Close(data)).unwrap();
                          future::err(ServerError::Closed)
                      }),
                      _ => {
                          debug!("Warning: other types of frames ignored");
                          Either::B(future::ok(childin))
                      }
                  }
              }
        )
        .map(|_| debug!("Dropping from_ws_to_child"))
        .map_err(|e| {
            if let ServerError::Closed = e {
                debug!("Connection closed")
            } else {
                error!("During from_ws_to_child: {}", e)
            }
        })
}

/// Process tx messages.
///
/// When a Close message is received, a Close frame is sent to the
/// client.
fn from_tx_to_ws(
    rx: mpsc::UnboundedReceiver<TxMessage>,
    child: Child,
    netout: impl Sink<SinkItem = Chunk, SinkError = EncodeError>
) -> impl Future<Item = (), Error = ()>
{
    rx.map_err(|()| Error::from(io::Error::new(io::ErrorKind::Other, "")))
        .fold((child, netout), |(child, netout), msg| {
            match msg {
                TxMessage::ChildOutput(data) => {
                    debug!("from child: {:?}", data);
                    netout.send(Chunk::Data(data))
                }
                TxMessage::Close(_) => {
                    debug!("closing");
                    netout.send(Chunk::Close(BytesMut::from("")))
                }
            }.map(|netout| (child, netout))
        })
        .map(|_| debug!("Dropping from_tx_to_ws"))
        .map_err(|e| error!("During from_tx_to_ws: {}", e))
}

enum TxMessage {
    ChildOutput(BytesMut),
    Close(BytesMut)
}

#[derive(Debug, Fail)]
enum ServerError {
    #[fail(display = "Connection closed cleanly")]
    Closed,

    #[fail(display = "Encode error: {}", _0)]
    EncodeError(EncodeError),

    #[fail(display = "Decode error: {}", _0)]
    DecodeError(DecodeError),

    #[fail(display = "I/O error: {}", _0)]
    IOError(io::Error)
}

impl From<EncodeError> for ServerError {
    fn from(e: EncodeError) -> ServerError {
        ServerError::EncodeError(e)
    }
}

//! websocketd is a work-in-progress server implementation of the
//! WebSocket protocol as defined in RFC 6455, which implements a
//! reasonable subset of WebSocket. A full implementation is not
//! planned but possible.
//!
//! Details of websocketd and informal descriptions of the WebSocket
//! protocol are documented in the module-level documentation.

extern crate bytes;
extern crate tokio;
extern crate futures;
extern crate sha1;
extern crate base64;

#[macro_use]
extern crate failure;
#[macro_use]
extern crate failure_derive;

pub mod stream;
pub mod handshake;
pub mod frames;
pub mod messages;

use failure::Error;
use tokio::prelude::*;
use tokio::net::{TcpListener, TcpStream};
use tokio::codec::Decoder;
use bytes::BytesMut;
use messages::Message;

fn main() -> Result<(), Error> {
    let local_addr = "127.0.0.1:54321".parse()?;
    let listener = TcpListener::bind(&local_addr)?;
    let server = listener.incoming().for_each(|stream| {
        println!("{:?}", stream);
        echo_server(stream);
        Ok(())
    }).map_err(|e| println!("Error: {:?}", e));
    tokio::run(server);
    Ok(())
}

fn echo_server(stream: TcpStream) {
    let conn = handshake::handshake(stream)
        .and_then(|cloneable_stream| Ok(cloneable_stream.into_inner()))
        .and_then(|stream| {
            let codec = messages::MessagesCodec::new().framed(stream);
            let (writer, reader) = codec.split();

            reader
                .map_err(Error::from)
                .fold(writer, |writer, message| {
                    println!("{:?}", message);
                    let reply = match message {
                        Message::Text(bytes) => {
                            Message::Text(bytes)
                        }
                        Message::Binary(_payload) => {
                            Message::Text(BytesMut::from("<binary>"))
                        }
                        Message::Close(_payload) => {
                            Message::Close(BytesMut::new())
                        }
                        Message::Ping(payload) => {
                            // MUST have the same payload
                            Message::Pong(payload)
                        }
                        _ => {
                            Message::Text(BytesMut::from("pong"))
                        }
                    };
                    writer.send(reply)
                        .and_then(Sink::flush)
                })
                .map(|_| ())
                .map_err(|e| println!("Messages: {:?}", e))
        });

    tokio::spawn(conn);
}

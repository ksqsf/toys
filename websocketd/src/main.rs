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

pub mod tcp_stream;
pub mod handshake;
pub mod frames;
pub mod messages;
pub mod streams;

use failure::Error;
use futures::future::Either;
use tokio::prelude::*;
use tokio::net::{TcpListener, TcpStream};
use tokio::codec::Decoder;
use bytes::BytesMut;
// use messages::{Message, EncodeError};
use streams::{Chunk, EncodeError};

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
            let codec = streams::StreamingCodec::new().framed(stream);
            let (writer, reader) = codec.split();

            reader
                .map_err(Error::from)
                .fold(writer, process_message)
                .map(|_| ())
                .map_err(|e| println!("During message loop: {}", e))
        });

    tokio::spawn(conn);
}

/// Process a chunk, resolving to the sink itself.
fn process_message<O>(
    sink: O,
    chunk: Chunk
) -> impl Future<Item = O, Error = EchoError>
where
    O: Sink<SinkItem = Chunk, SinkError = EncodeError>
{
    println!("{:?}", chunk);
    let reply = match chunk {
        Chunk::Data(bytes) => Chunk::Data(bytes),
        Chunk::Close(bytes) => Chunk::Close(bytes),
        Chunk::Ping(bytes) => Chunk::Pong(bytes),
        Chunk::Pong(_) => Chunk::Data(BytesMut::from("pong"))
    };

    if let Chunk::Close(_) = reply {
        // Cleanly close WebSocket connection
        Either::B(
            sink.send(reply)
                .and_then(Sink::flush)
                .then(|res| {
                    match res {
                        Ok(_) => Err(EchoError::Closed),
                        Err(e) => Err(EchoError::from(e)),
                    }
                })
        )
    } else {
        Either::A(
            sink.send(reply)
                .and_then(Sink::flush)
                .map(|w| { println!("send ok"); w })
                .map_err(EchoError::from)
        )
    }
}

#[derive(Debug, Fail)]
enum EchoError {
    #[fail(display = "Connection closed cleanly")]
    Closed,

    #[fail(display = "Encode error: {}", _0)]
    EncodeError(EncodeError)
}

impl From<EncodeError> for EchoError {
    fn from(e: EncodeError) -> EchoError {
        EchoError::EncodeError(e)
    }
}

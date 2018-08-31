//! websocketd is a work-in-progress server implementation of the
//! WebSocket protocol as defined in RFC 6455, which implements a
//! reasonable subset of WebSocket. A full implementation is not
//! planned but possible.
//!
//! Details of websocketd and informal descriptions of the WebSocket
//! protocol are documented in the module-level documentation.

extern crate bytes;
extern crate tokio;
extern crate tokio_process;
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

use std::io;
use std::env;
use std::process::{Command, Stdio};
use failure::Error;
use tokio::prelude::*;
use tokio::net::{TcpListener, TcpStream};
use tokio::codec::Decoder;
use tokio::codec::{BytesCodec, FramedRead};
use tokio_process::CommandExt;
// use messages::{Message, EncodeError};
use streams::{Chunk, EncodeError};

fn main() -> Result<(), Error> {
    let local_addr = "127.0.0.1:54321".parse()?;
    let listener = TcpListener::bind(&local_addr)?;

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a command");
        return Ok(())
    }

    let server = listener.incoming().for_each(move |stream| {
        println!("{:?}", stream);
        if let Err(e) = pipe_server(stream, &args) {
            println!("Error happened when starting pipe service for client: {}", e);
        }
        Ok(())
    }).map_err(|e| println!("Error: {:?}", e));

    tokio::run(server);
    Ok(())
}

fn pipe_server(stream: TcpStream, args: &Vec<String>) -> Result<(), io::Error> {
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
    child.forget();

    let conn = handshake::handshake(stream)
        .and_then(|cloneable_stream| Ok(cloneable_stream.into_inner()))
        .and_then(|stream| {
            let codec = streams::StreamingCodec::new(true).framed(stream);
            let (netout, netin) = codec.split();

            let from_ws_to_child = netin
                .fold(childin, |mut childin, chunk| {
                    println!("from ws: {:?}", chunk);
                    match chunk {
                        Chunk::Data(data) => {
                            if let Err(e) = childin.poll_write(&data) {
                                return Err(e)
                            };
                            childin.poll_flush()?;
                        }
                        _ => println!("Warning: Other types of chunks ignored")
                    };
                    Ok(childin)
                })
                .map(|_| println!("Dropping from_ws_to_child"))
                .map_err(|e| println!("From WS to child: {}", e));

            let from_child_to_ws = FramedRead::new(childout, BytesCodec::new())
                .map_err(EncodeError::from)
                .fold(netout, |netout, bytes| {
                    println!("from child: {:?}", bytes);
                    netout.send(Chunk::Data(bytes))
                })
                .map(|_| println!("Dropping from_child_to_ws"))
                .map_err(|e| println!("From child to WS: {}", e));

            tokio::spawn(from_ws_to_child);
            tokio::spawn(from_child_to_ws);

            Ok(())
        });

    tokio::spawn(conn);
    Ok(())
}

#[derive(Debug, Fail)]
enum ServerError {
    #[fail(display = "Connection closed cleanly")]
    _Closed,

    #[fail(display = "Encode error: {}", _0)]
    EncodeError(EncodeError)
}

impl From<EncodeError> for ServerError {
    fn from(e: EncodeError) -> ServerError {
        ServerError::EncodeError(e)
    }
}

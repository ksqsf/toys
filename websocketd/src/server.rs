//! # Server
//!
//! The server module is responsible for all bussiness logic.

use std::io;
use std::process::{Command, Stdio};
use tokio;
use tokio::prelude::*;
use tokio::codec::Decoder;
use tokio::codec::{BytesCodec, FramedRead};
use tokio::net::{TcpListener, TcpStream};
use tokio_process::CommandExt;

use handshake::handshake;
use streams::{self, Chunk, EncodeError};

pub fn server(
    listener: TcpListener,
    args: Vec<String>
) -> impl Future<Item = (), Error = ()>
{
    listener.incoming().for_each(move |stream| {
        if let Err(e) = do_pipe(stream, &args) {
            println!("Error happened when starting pipe service for client: {}", e)
        }
        future::ok(())
    }).map_err(|e| println!("Error processing clients: {}", e))
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
                .fold((child, netout), |(child, netout), bytes| {
                    println!("from child: {:?}", bytes);
                    netout.send(Chunk::Data(bytes))
                        .map(|netout| (child, netout))
                })
                .map(|_| println!("Dropping from_child_to_ws"))
                .map_err(|e| println!("From child to WS: {}", e));

            tokio::spawn(from_ws_to_child);
            tokio::spawn(from_child_to_ws);

            future::ok(())
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

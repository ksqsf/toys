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
pub mod server;

use std::env;
use failure::Error;
use tokio::net::TcpListener;

fn main() -> Result<(), Error> {
    let local_addr = "127.0.0.1:54321".parse()?;
    let listener = TcpListener::bind(&local_addr)?;

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Please provide a command");
    } else {
        tokio::run(server::server(listener, args));
    }

    Ok(())
}

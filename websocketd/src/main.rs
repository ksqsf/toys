extern crate tokio;
extern crate futures;
extern crate sha1;
extern crate base64;

#[macro_use]
extern crate failure;

use failure::Error;
use tokio::prelude::*;
use tokio::net::{TcpListener, TcpStream};

mod handshake;

fn main() -> Result<(), Error> {
    let local_addr = "127.0.0.1:54321".parse()?;
    let listener = TcpListener::bind(&local_addr)?;
    let server = listener.incoming().for_each(|stream| {
        println!("{:?}", stream);
        handle_connection(stream);
        Ok(())
    }).map_err(|e| println!("Error: {:?}", e));
    tokio::run(server);
    Ok(())
}

fn handle_connection(stream: TcpStream) {
    let conn = handshake::handshake(stream)
        .and_then(|_| {
            println!("Closing");
            Ok(())
        });

    tokio::spawn(conn);
}

extern crate tokio;
extern crate futures;
extern crate sha1;
extern crate base64;

#[macro_use]
extern crate failure;

mod stream;
mod handshake;

use failure::Error;
use tokio::prelude::*;
use tokio::net::TcpListener;
use stream::TcpStream;

fn main() -> Result<(), Error> {
    let local_addr = "127.0.0.1:54321".parse()?;
    let listener = TcpListener::bind(&local_addr)?;
    let server = listener.incoming().for_each(|stream| {
        println!("{:?}", stream);
        handle_connection(TcpStream::new(stream));
        Ok(())
    }).map_err(|e| println!("Error: {:?}", e));
    tokio::run(server);
    Ok(())
}

fn handle_connection(stream: TcpStream) {
    let conn = handshake::handshake(stream)
        .and_then(|mut _stream| {
            println!("Closing");
            Ok(())
        });

    tokio::spawn(conn);
}

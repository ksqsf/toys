extern crate bytes;
extern crate tokio;
extern crate futures;
extern crate sha1;
extern crate base64;

#[macro_use]
extern crate failure;

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

            tokio::spawn(
                writer.send(Message::Text(BytesMut::from("Hello, world")))
                    .map(|_| ())
                    .map_err(|e| println!("During send: {:?}", e))
            );

            reader
                .for_each(|message| {
                    println!("{:?}", message);
                    Ok(())
                })
                .map_err(|e| println!("Messages: {:?}", e))
        });

    tokio::spawn(conn);
}

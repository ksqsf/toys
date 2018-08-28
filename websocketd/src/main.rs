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
                .map_err(|e| Error::from(e))
                .fold(writer.wait(), |mut writer, message| {
                    println!("{:?}", message);
                    match message {
                        Message::Text(bytes) => {
                            if let Err(e) = writer.send(Message::Text(bytes)) {
                                return Err(Error::from(e))
                            }
                            if let Err(e) = writer.flush() {
                                return Err(Error::from(e))
                            }
                        }
                        Message::Binary(_payload) => {
                            if let Err(e) = writer.send(Message::Text(BytesMut::from("<binary>"))) {
                                return Err(Error::from(e))
                            }
                        }
                        Message::Control(opcode, _payload) => {
                            println!("Control frame with opcode={}", opcode as u8)
                        }
                    }
                    Ok(writer)
                })
                .map(|_| ())
                .map_err(|e| println!("Messages: {:?}", e))
        });

    tokio::spawn(conn);
}

//! # Handshake
//!
//! WebSocket handshaking is layered above HTTP and makes use of HTTP
//! upgrades.  It's designed to avoid attacks initiaied inside a Web
//! browser.
//!
//! ## Client behaviors
//! The client will request the server like this
//! ```text
//! GET /chat HTTP/1.1
//! Upgrade: websocket
//! Connection: Upgrade
//! Host: example.com
//! Origin: http://example.com
//! Sec-WebSocket-Key: <key>
//! Sec-WebSocket-Version: 13
//! ```
//!
//! ## Server behaviors
//! A successful handshake response:
//! ```text
//! HTTP/1.1 101 Switching Protocols
//! Upgrade: websocket
//! Connection: Upgrade
//! Sec-WebSocket-Accept: <a generated key>
//! ```
//!
//! When errors occur, an appropriate HTTP error status can be
//! returned.
use std::sync::{Arc, Mutex};
use std::collections::BTreeMap;
use failure::Error;
use futures::future::Either;
use tokio::{
    prelude::*,
    codec::{
        Decoder,
        LinesCodec
    },
    net::TcpStream
};
use sha1::Sha1;
use base64;
use stream::CloneableTcpStream;

/// Create a future which tries to handshake with `TcpStream` and, if
/// successful, evaluates to the stream itself.
///
/// Note that current implementation will consume the error and log it
/// instead of propagating.
///
/// On the process of handshaking, see [module-level](./index.html)
/// documentation for more.
pub fn handshake(stream: TcpStream)
    -> impl Future<Item = CloneableTcpStream, Error = ()>
{
    let cloneable_stream = CloneableTcpStream::new(stream);

    let (sink, lines) = LinesCodec::new()
        .framed(cloneable_stream.clone())
        .split();

    lines
        .into_future()
        .map_err(|(e, _)| Error::from(e))
        .and_then(|(first, lines)| {
            let first = match first {
                Some(first) => first,
                None => return Either::A(future::err(format_err!("Client exit early")))
            };
            println!("First line: {}", first);

            // Check validity of the request and extract resource
            let resource = match extract_resource(&first) {
                Ok(r) => r,
                Err(e) => return Either::A(future::err(e))
            };

            // Create a new request builder
            let builder = Arc::new(Mutex::new(HttpRequest {
                resource: resource.to_string(),
                headers: BTreeMap::new()
            }));

            // Collect headers
            let continuation = lines
                .map_err(Error::from)
                .take_while(|line| Ok(line != ""))
                .fold(builder, |builder, line| {
                    println!("header = {}", line);
                    let parts: Vec<&str> = line.split(": ").collect();
                    if parts.len() != 2 {
                        bail!("Invalid header")
                    }
                    builder
                        .lock()
                        .unwrap()
                        .headers
                        .insert(
                            parts[0].trim().to_string(),
                            parts[1].trim().to_string()
                        );
                    Ok(builder)
                })
                .and_then(|req| {
                    println!("Request = {:#?}", req);

                    // TODO: various checks including Connection, Origin, Hostname
                    // For now, just let it go

                    let req = req.lock().unwrap();
                    let key = match req.headers.get("Sec-WebSocket-Key") {
                        Some(key) => key,
                        None => bail!("Sec-WebSocket-Key is missing")
                    };

                    let mut m = Sha1::new();
                    m.update(key.as_bytes());
                    m.update(b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
                    let sha1sum = m.digest().bytes();
                    let secret = base64::encode(&sha1sum);
                    println!("secret={}", secret);

                    Ok(secret)
                })
                .and_then(|secret| {
                    sink.send(format!(concat!(
                        "HTTP/1.1 101 Switching Protocols\r\n",
                        "Upgrade: websocket\r\n",
                        "Connection: Upgrade\r\n",
                        "Sec-WebSocket-Accept: {}\r\n",
                        "\r"
                    ), secret))
                        .map(|_| cloneable_stream)
                        .map_err(Error::from)
                });

            Either::B(continuation)
        })
        .map_err(|e| println!("During handshake: {:?}", e))
}

fn extract_resource(request: &str) -> Result<&str, Error> {
    let parts: Vec<&str> = request.split(' ').collect();
    if parts.len() != 3 {
        bail!("Invalid request")
    }
    if parts[2] != "HTTP/1.1" {
        bail!("HTTP version is not 1.1")
    }
    if parts[0] != "GET" {
        bail!("HTTP method is not GET")
    }
    Ok(parts[1])
}

#[derive(Clone, Debug)]
struct HttpRequest {
    resource: String,
    headers: BTreeMap<String, String>
}

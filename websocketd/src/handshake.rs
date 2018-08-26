use std::sync::{Arc, Mutex};
use std::collections::BTreeMap;
use failure::Error;
use futures::future::Either;
use tokio::{
    prelude::*,
    executor::spawn,
    net::TcpStream,
    codec::{
        Decoder,
        LinesCodec
    }
};

pub fn handshake(stream: TcpStream) {
    let handshaker = LinesCodec::new()
        .framed(stream)
        .into_future()
        .map_err(|(e, _)| Error::from(e))
        .and_then(|(first, lines)| {
            let first = match first {
                Some(first) => first,
                None => return Either::A(future::ok(()))
            };
            println!("First line: {}", first);

            // Check validity of the request and extract resource
            let resource = match extract_resource(&first) {
                Ok(r) => r,
                Err(e) => return Either::A(future::err(e))
            };

            // Create a new builder
            let builder = Arc::new(Mutex::new(HttpRequest {
                resource: resource.to_string(),
                headers: BTreeMap::new()
            }));

            // Collect headers
            let headers = lines
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
                    Ok(())
                });
            Either::B(headers)
        })
        .map_err(|e| println!("During handshake: {:?}", e));
    spawn(handshaker);
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

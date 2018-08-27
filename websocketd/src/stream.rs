//! # `Clone`-able `TcpStream`
//!
//! This module contains a newtype wrapper called `CloneableTcpStream`
//! around `tokio::new::TcpStream`, and implements essential async IO
//! traits.  It supports `clone` and is thread-safe.

use std::sync::{Arc, Mutex};
use tokio::prelude::*;
use tokio::io::Error;
use tokio::net::TcpStream;

/// A thread-safe cloneable asynchronous TcpStream.
///
/// See module-level documentation for more.
#[derive(Debug, Clone)]
pub struct CloneableTcpStream(pub Arc<Mutex<TcpStream> >);

impl CloneableTcpStream {
    pub fn new(inner: TcpStream) -> Self {
        CloneableTcpStream(Arc::new(Mutex::new(inner)))
    }

    pub fn into_inner(self) -> TcpStream {
        Arc::try_unwrap(self.0)
            .unwrap()
            .into_inner()
            .unwrap()
    }
}

impl Read for CloneableTcpStream {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
        self.0.lock().unwrap().read(buf)
    }
}

impl Write for CloneableTcpStream {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        self.0.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> Result<(), Error> {
        self.0.lock().unwrap().flush()
    }
}

impl AsyncRead for CloneableTcpStream {}

impl AsyncWrite for CloneableTcpStream {
    fn shutdown(&mut self) -> Result<Async<()>, Error> {
        // FIXME: is this correct?
        let m = Arc::get_mut(&mut self.0).unwrap().get_mut().unwrap();
        <TcpStream as AsyncWrite>::shutdown(m)
    }
}

//! # Message encoding and decoding
//!
//! In reality, both client and server work with messages instead of
//! raw byte stream or frame stream. However, WebSocket supports
//! fragmentation of messages, where a single message can be split
//! into multiple frames. This module provides several types to enable
//! you to work with messages, and contains a codec that can compose
//! multiple frames into a single message.
//!
//! This codec doesn't do anything with control frames and return them
//! immediately. Data frames are accumulated into one single message.
//!
//! ## Behaviors
//!
//! 1. A control message always fits in a single frame.
//! 2. A control message can appear any time.
//! 3. No interleaved data messages allowed.
//! 4. Frame boundaries are not trustable unless extension is negotiated.

use std::mem;
use std::io::Error;
use bytes::BytesMut;
use tokio::codec::{Decoder, Encoder};
use frames::{self, Frame, FramesCodec};

pub use frames::Opcode;

/// WebSocket message.
#[derive(Debug, Clone)]
pub enum Message {
    Text(BytesMut),
    Binary(BytesMut),
    Ping(BytesMut),
    Pong(BytesMut),
    Close(BytesMut)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DecodeState {
    // Ready for a new message
    Complete,

    // Message is not complete yet; more frames are needed
    Incomplete
}

#[derive(Clone, Debug)]
pub struct MessagesCodec {
    opcode: Opcode,
    frames_codec: FramesCodec,
    state: DecodeState,
    buffer: BytesMut
}

impl MessagesCodec {
    pub fn new() -> Self {
        Self {
            opcode: Opcode::Continuation,
            frames_codec: FramesCodec::new(),
            state: DecodeState::Complete,
            buffer: BytesMut::with_capacity(1024*1024*10)
        }
    }
}

impl Decoder for MessagesCodec {
    type Item = Message;
    type Error = DecodeError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Message>, DecodeError> {
        use self::DecodeState::*;

        loop {
            let frame = self.frames_codec.decode(src)?;
            if let None = frame {
                return Ok(None)
            }

            let frame = frame.unwrap();

            // Control frames are returned right away.
            if frame.is_control() {
                return Ok(Some(match frame.opcode {
                    Opcode::Close => Message::Close(frame.app_data),
                    Opcode::Ping => Message::Ping(frame.app_data),
                    Opcode::Pong => Message::Pong(frame.app_data),
                    _ => unreachable!()
                }))
            }

            // Interleaved frames are invalid.
            if !frame.is_continuation() && self.state == Incomplete {
                return Err(DecodeError::InterleavedMessages)
            }

            // Start of a message.
            if !frame.is_continuation() {
                self.state = Incomplete;
                self.opcode = frame.opcode;
            }

            self.buffer.extend(frame.app_data);

            // End of a message
            if frame.fin {
                self.state = Complete;
                let data = mem::replace(&mut self.buffer, BytesMut::with_capacity(1024));
                return match self.opcode {
                    Opcode::Text => Ok(Some(Message::Text(data))),
                    Opcode::Binary => Ok(Some(Message::Binary(data))),
                    _ => unreachable!()
                }
            }
        }
    }
}

impl Encoder for MessagesCodec {
    type Item = Message;
    type Error = EncodeError;

    fn encode(&mut self, msg: Message, sink: &mut BytesMut) -> Result<(), EncodeError> {
        let (opcode, payload) = match msg {
            Message::Text(payload) => (Opcode::Text, payload),
            Message::Binary(payload) => (Opcode::Binary, payload),
            Message::Close(payload) => (Opcode::Close, payload),
            Message::Ping(payload) => (Opcode::Ping, payload),
            Message::Pong(payload) => (Opcode::Pong, payload)
        };

        let mut frame: Frame = Default::default();
        frame.fin = true;
        frame.opcode = opcode;
        frame.payload_len = payload.len() as u64;
        frame.app_data = payload;

        self.frames_codec.encode(frame, sink)
            .map_err(EncodeError::from)
    }
}

#[derive(Fail, Debug)]
pub enum DecodeError {
    #[fail(display = "Interleaved messages")]
    InterleavedMessages,

    #[fail(display = "Frame decode error: {}", _0)]
    FrameDecodeError(frames::DecodeError),

    #[fail(display = "IO error: {}", _0)]
    IOError(Error)
}

impl From<Error> for DecodeError {
    fn from(e: Error) -> DecodeError {
        DecodeError::IOError(e)
    }
}

impl From<frames::DecodeError> for DecodeError {
    fn from(e: frames::DecodeError) -> DecodeError {
        DecodeError::FrameDecodeError(e)
    }
}

#[derive(Fail, Debug)]
pub enum EncodeError {
    #[fail(display = "Frame encode error: {}", _0)]
    FrameEncodeError(frames::EncodeError),

    #[fail(display = "IO error: {}", _0)]
    IOError(Error)
}

impl From<Error> for EncodeError {
    fn from(e: Error) -> EncodeError {
        EncodeError::IOError(e)
    }
}

impl From<frames::EncodeError> for EncodeError {
    fn from(e: frames::EncodeError) -> EncodeError {
        EncodeError::FrameEncodeError(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::BufMut;

    #[test]
    fn decode_single() {
        let mut buf = BytesMut::new();
        let mut codec = MessagesCodec::new();
        buf.put_u8(0b1000_0001);
        buf.put_u8(0b0000_0001);
        buf.put_u8(b'a');
        let msg = codec.decode(&mut buf).unwrap().unwrap();
        match msg {
            Message::Text(payload) => assert_eq!(payload[0], b'a'),
            _ => unreachable!()
        }
    }

    #[test]
    fn decode_two() {
        let mut buf = BytesMut::new();
        let mut codec = MessagesCodec::new();
        buf.put_u8(0b0000_0001);
        buf.put_u8(0b0000_0001);
        buf.put_u8(b'a');
        buf.put_u8(0b1000_0000);
        buf.put_u8(0b0000_0001);
        buf.put_u8(b'b');
        let msg = codec.decode(&mut buf).unwrap().unwrap();
        match msg {
            Message::Text(payload) => {
                assert_eq!(payload[0], b'a');
                assert_eq!(payload[1], b'b');
            }
            _ => unreachable!()
        }
    }

    #[test]
    fn encode() {
        let msg = Message::Text(BytesMut::from("a"));
        let mut buf = BytesMut::new();
        let mut codec = MessagesCodec::new();
        codec.encode(msg, &mut buf).unwrap();
        assert_eq!(buf[0], 0b1000_0001);
        assert_eq!(buf[1], 0b0000_0001);
        assert_eq!(buf[2], b'a');
    }
}

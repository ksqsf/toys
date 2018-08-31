//! # Streaming codec
//!
//! Typically, a server does not need to buffer a whole message before
//! moving on.  Streaming API can handle data frames as soon as they
//! arrives, enhancing performance.
//!
//! This streaming codec assumes **there are no interleaved data
//! messages**, and **there are no interleaved frames.** It works on
//! the frame level and just passes payload data as-is.

use std::io;
use bytes::BytesMut;
use tokio::codec::{Decoder, Encoder};

pub use frames::{self, Opcode, Frame, FramesCodec};
pub use frames::EncodeError;

/// Represents a chunk of data in stream.
#[derive(Debug, Clone)]
pub enum Chunk {
    Close(BytesMut),
    Ping(BytesMut),
    Pong(BytesMut),

    /// A unified variant for both text and binary.
    Data(BytesMut)
}

pub struct StreamingCodec {
    opcode: Opcode,
    payload_len: usize,
    mask: Option<[u8; 4]>,

    /// How many bytes of data to be read to finish this frame?
    ///
    /// 0 means a frame has been completed.
    remaining: usize,

    frames_codec: FramesCodec
}

impl StreamingCodec {
    pub fn new() -> Self {
        StreamingCodec {
            opcode: Opcode::Continuation,
            payload_len: 0,
            mask: None,
            remaining: 0,
            frames_codec: FramesCodec::new()
        }
    }
}

impl Decoder for StreamingCodec {
    type Item = Chunk;
    type Error = DecodeError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Chunk>, DecodeError> {
        loop {
            // No more data to process.
            if src.len() == 0 {
                return Ok(None)
            }

            if self.remaining > 0 {
                // Need more data to complete this frame.
                let n = self.remaining.min(src.len());
                let mut payload = src.split_to(n);
                if let Some(mask) = self.mask {
                    for (i, c) in payload.iter_mut().enumerate() {
                        *c ^= mask[(self.payload_len - self.remaining + i) % 4];
                    }
                }
                self.remaining -= n;
                let chunk = Chunk::Data(payload);
                return Ok(Some(chunk))
            }

            // A new frame. Determine its opcode.
            let opcode = match Opcode::try_from_u8(src[0] & 0b1111) {
                Some(o) => o,
                None => return Err(DecodeError::InvalidOpcode(src[0] & 0b1111))
            };

            // A new control frame? Read it as a whole and return.
            if opcode.is_control() {
                let frame = self.frames_codec.decode(src)?;
                let frame = match frame {
                    Some(f) => f,
                    None => return Ok(None)
                };
                let chunk = match opcode {
                    Opcode::Close => Chunk::Close(frame.app_data),
                    Opcode::Ping => Chunk::Ping(frame.app_data),
                    Opcode::Pong => Chunk::Pong(frame.app_data),
                    _ => unreachable!()
                };
                return Ok(Some(chunk))
            }

            // A new data frame? Determine its payload length and mask.
            self.opcode = opcode;
            let mut header_len: usize = 2;
            if src.len() < 2 {
                return Ok(None)
            }
            let masked = (src[1] & 0b1000_0000) != 0;
            let mask_len = if masked { 4 } else { 0 };
            let mut payload_len: usize = (src[1] & 0b01111111) as usize;

            if payload_len == 126 && src.len() < 4 + mask_len
                || payload_len == 127 && src.len() < 10 + mask_len
            {
                return Ok(None)
            }
            if payload_len == 126 {
                header_len += 2;
                payload_len = (src[2] as usize) << 8 | (src[3] as usize);
            }
            else if payload_len == 127 {
                header_len += 8;
                payload_len =
                    (src[2] as usize) << 56
                    | (src[3] as usize) << 48
                    | (src[4] as usize) << 40
                    | (src[5] as usize) << 32
                    | (src[6] as usize) << 24
                    | (src[7] as usize) << 16
                    | (src[8] as usize) << 8
                    | (src[9]) as usize;
            }
            self.payload_len = payload_len;
            self.remaining = payload_len;

            if masked {
                self.mask = Some([
                    src[header_len],
                    src[header_len + 1],
                    src[header_len + 2],
                    src[header_len + 3]
                ]);
                header_len += 4;
            } else {
                self.mask = None;
            }

            src.advance(header_len);
        }
    }
}

impl Encoder for StreamingCodec {
    type Item = Chunk;
    type Error = EncodeError;

    fn encode(&mut self, chunk: Chunk, sink: &mut BytesMut) -> Result<(), EncodeError> {
        self.frames_codec.encode(chunk.into(), sink)
    }
}

impl From<Chunk> for Frame {
    /// Convert a Chunk to a Frame.  Note that Data chunks are
    /// converted to binary frames, which is because a Text frame must
    /// contain only valid UTF-8 text, which is not the case here.
    fn from(chunk: Chunk) -> Frame {
        match chunk {
            Chunk::Close(b) => Frame {
                fin: true,
                opcode: Opcode::Close,
                payload_len: b.len() as u64,
                app_data: b,
                ..Frame::default()
            },
            Chunk::Ping(b) => Frame {
                fin: true,
                opcode: Opcode::Ping,
                payload_len: b.len() as u64,
                app_data: b,
                ..Frame::default()
            },
            Chunk::Pong(b) => Frame {
                fin: true,
                opcode: Opcode::Pong,
                payload_len: b.len() as u64,
                app_data: b,
                ..Frame::default()
            },
            Chunk::Data(b) => Frame {
                fin: true,
                opcode: Opcode::Binary,
                payload_len: b.len() as u64,
                app_data: b,
                ..Frame::default()
            }
        }
    }
}

#[derive(Debug, Fail)]
pub enum DecodeError {
    #[fail(display = "Invalid opcode {}", _0)]
    InvalidOpcode(u8),

    #[fail(display = "Cannot decode a frame: {}", _0)]
    FrameDecodeError(frames::DecodeError),

    #[fail(display = "IO error: {}", _0)]
    IOError(io::Error)
}

impl From<io::Error> for DecodeError {
    fn from(e: io::Error) -> DecodeError {
        DecodeError::IOError(e)
    }
}

impl From<frames::DecodeError> for DecodeError {
    fn from(e: frames::DecodeError) -> DecodeError {
        DecodeError::FrameDecodeError(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bytes::BufMut;

    #[test]
    fn decode1() {
        let mut buf = BytesMut::new();
        let mut codec = StreamingCodec::new();
        buf.put_u8(0b1000_0001); // fin, text
        buf.put_u8(0b1000_0010); // masked, len=2
        buf.put_u8(0); // mask
        buf.put_u8(0);
        buf.put_u8(0);
        buf.put_u8(0);
        buf.put_u8(b'a' ^ 0);
        let chunk = codec.decode(&mut buf).unwrap().expect("a2");
        if let Chunk::Data(payload) = chunk {
            assert_eq!(payload[0], b'a');
        } else {
            panic!("not data")
        }
        buf.put_u8(b'x' ^ 0);
        let chunk = codec.decode(&mut buf).unwrap().expect("x2");
        if let Chunk::Data(payload) = chunk {
            assert_eq!(payload[0], b'x');
        } else {
            panic!("not data")
        }
        let chunk = codec.decode(&mut buf).unwrap();
        assert!(chunk.is_none());
    }
}

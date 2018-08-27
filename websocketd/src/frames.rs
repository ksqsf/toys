//! # Frame encoding and decoding
//!
//! WebSocket is "almost" raw TCP connection, except that it
//! introduces its own frame type.
//!
//! This module provides several types and enums to work with
//! WebSocket frames, and implements frame encoding and decoding.
//!
//! The authoritive figure for WebSocket framing protocol is attached here for reference.
//!
//! ```text
//!  0                   1                   2                   3   
//!  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 
//! +-+-+-+-+-------+-+-------------+-------------------------------+
//! |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
//! |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
//! |N|V|V|V|       |S|             |   (if payload len==126/127)   |
//! | |1|2|3|       |K|             |                               |
//! +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
//! |     Extended payload length continued, if payload len == 127  |
//! + - - - - - - - - - - - - - - - +-------------------------------+
//! |                               |Masking-key, if MASK set to 1  |
//! +-------------------------------+-------------------------------+
//! | Masking-key (continued)       |          Payload Data         |
//! +-------------------------------- - - - - - - - - - - - - - - - +
//! :                     Payload Data continued ...                :
//! + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
//! |                     Payload Data continued ...                |
//! +---------------------------------------------------------------+
//! ```
//!
//! A few things to note:
//! 1. Extension data is not supported.

use std::u16;
use std::io::Error;
use tokio::codec::Decoder;
use tokio::codec::Encoder;
use bytes::{BytesMut, BufMut};

/// Opcodes defined as in the specification.
///
/// Represented as `u8`
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    Continutation = 0,
    Text,
    Binary,
    Close = 8,
    Ping,
    Pong
}

impl Default for Opcode {
    fn default() -> Opcode {
        Opcode::Close
    }
}

/// Representation of a frame.
///
/// See [module-level](./index.html) documentation for the physical
/// format.
#[derive(Debug)]
pub struct Frame {
    fin: bool,           // 1 bit
    rsv1: bool,          // 1 bit
    rsv2: bool,          // 1 bit
    rsv3: bool,          // 1 bit
    opcode: Opcode,      // 4 bits (1 byte so far)
    mask: bool,          // 1 bit
    payload_len: u64,    // 7 bits + 0|2|8 bytes
    mask_key: [u8; 4],       // 0|4 bytes
    // ext_data: BytesMut,  // x bytes
    app_data: BytesMut,  // y bytes
}

#[derive(Debug)]
pub enum DecodeError {
    InvalidOpcodeError,
    IOError(Error)
}

impl From<Error> for DecodeError {
    fn from(e: Error) -> DecodeError {
        DecodeError::IOError(e)
    }
}

#[derive(Debug)]
pub enum EncodeError {
    IOError(Error)
}

impl From<Error> for EncodeError {
    fn from(e: Error) -> EncodeError {
        EncodeError::IOError(e)
    }
}

pub struct FramesCodec {
    /// Control part + payload_len + mask_key
    header_len: Option<u64>,

    /// Known payload length
    payload_len: Option<u64>
}

impl FramesCodec {
    fn get_payload_len_and_construct(&mut self, src: &mut BytesMut) -> Result<Option<Frame>, DecodeError> {
        debug_assert!(src.len() >= 2);

        let mut payload_len: u64 = (src[1] & 0b01111111) as u64;
        if payload_len == 126 && src.len() < 4 || payload_len == 127 && src.len() < 10 {
            return Ok(None)
        }
        if payload_len == 126 {
            self.header_len = Some(2+2+4);
            payload_len =
                (src[2] << 8) as u64
                | (src[3]) as u64;
        } else if payload_len == 127 {
            self.header_len = Some(2 + 8 + 4);
            payload_len =
                (src[2] << 56) as u64
                | (src[3] << 48) as u64
                | (src[4] << 40) as u64
                | (src[5] << 32) as u64
                | (src[6] << 24) as u64
                | (src[7] << 16) as u64
                | (src[8] << 8) as u64
                | (src[9]) as u64;
        } else {
            self.header_len = Some(2+4);
        }
        self.payload_len = Some(payload_len);

        self.construct(src)
    }

    fn construct(&mut self, src: &mut BytesMut) -> Result<Option<Frame>, DecodeError> {
        let header_len = self.header_len.unwrap();
        let payload_len = self.payload_len.unwrap();

        if src.len()  < header_len as usize + payload_len as usize {
            return Ok(None)
        }

        let mut src = src.split_to(header_len as usize + payload_len as usize);
        let frame = Frame {
            fin: ((src[0]  >> 7) & 1) == 1,
            rsv1: ((src[0]  >> 6) & 1) == 1,
            rsv2: ((src[0]  >> 5) & 1) == 1,
            rsv3: ((src[0]  >> 4) & 1) == 1,
            opcode: match src[0] & 0b1111 {
                0 => Opcode::Continutation,
                1 => Opcode::Text,
                2 => Opcode::Binary,
                8 => Opcode::Close,
                9 => Opcode::Ping,
                10 => Opcode::Pong,
                _ => return Err(DecodeError::InvalidOpcodeError)
            },
            mask: ((src[1] >> 7) & 1) == 1,
            payload_len,
            mask_key: [src[header_len as usize - 4],
                       src[header_len as usize - 3],
                       src[header_len as usize - 2],
                       src[header_len as usize - 1]],
            app_data: src.split_off(header_len as usize)
        };
        self.header_len = None;
        self.payload_len = None;
        Ok(Some(frame))
    }
}

impl Decoder for FramesCodec {
    type Item = Frame;
    type Error = DecodeError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Frame>, DecodeError> {
        match self.payload_len {
            None if src.len() < 2 => Ok(None),
            None => self.get_payload_len_and_construct(src),
            Some(_) => self.construct(src)
        }
    }
}

impl Encoder for FramesCodec {
    type Item = Frame;
    type Error = EncodeError;

    fn encode(&mut self, frame: Frame, sink: &mut BytesMut) -> Result<(), EncodeError> {
        sink.reserve(2 + 8 + 4 + frame.payload_len as usize);

        sink.put_u8((frame.fin as u8) << 3  |
                    (frame.rsv1 as u8) << 2 |
                    (frame.rsv2 as u8) << 1 |
                    frame.rsv3 as u8);

        if frame.payload_len < 126 {
            sink.put_u8((frame.mask as u8) << 7 |
                        frame.payload_len as u8);
        } else if frame.payload_len <= u16::MAX as u64 {
            sink.put_u8((frame.mask as u8) << 7 | 126);
            sink.put_u16_be(frame.payload_len as u16);
        } else {
            sink.put_u8((frame.mask as u8) << 7 | 127);
            sink.put_u64_be(frame.payload_len);
        }

        sink.put(&frame.mask_key[..]);
        sink.put(&frame.app_data);

        Ok(())
    }
}

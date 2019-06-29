//! Defines the core types presenting the internal FIT structure.

use nom::Endianness;
use std::borrow::Cow;
use std::rc::Rc;

#[derive(Debug)]
pub enum RecordType {
    Definition,
    Data,
}

pub trait RecordHeader {
    fn compressed(&self) -> bool;
    fn local_type(&self) -> u8;
    fn record_type(&self) -> RecordType;
    fn offset(&self) -> u8;
    fn developer(&self) -> bool;
}

impl RecordHeader for u8 {
    #[inline(always)]
    fn compressed(&self) -> bool {
        (self & 0x80) == 0x80
    }
    #[inline(always)]
    fn local_type(&self) -> u8 {
        match self.compressed() {
            true => (self & 0x60) >> 5,
            false => self & 0x0f,
        }
    }
    #[inline(always)]
    fn record_type(&self) -> RecordType {
        match (self & 0xc0) == 0x40 {
            true => RecordType::Definition,
            false => RecordType::Data,
        }
    }
    #[inline(always)]
    fn offset(&self) -> u8 {
        match self.compressed() {
            true => self & 0x1f,
            false => 0,
        }
    }
    #[inline(always)]
    fn developer(&self) -> bool {
        (self & 0xe0) == 0x60
    }
}

/// A mandatory header located at the start of every Fit file.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct FileHeader {
    /// The total length of the header in bytes.
    pub length: u8,
    /// Protocol version number (see SDK).
    pub protocol: u8,
    /// Profile version number (see SDK).
    pub profile: u16,
    /// The length of the data section in bytes (ie. the length of the complete file, minus this
    /// header and the end-of-file checksum).
    pub file_size: u32,
    /// ASCII values for ".FIT".
    pub tag: [u8; 4],
    /// Optional checksum for this header. Verified if present.
    pub checksum: Option<u16>,
}

/// Defines the number, data-type, and length of a data field.
#[derive(Clone, Debug, PartialEq)]
pub struct FieldDefinition {
    /// Field number; identifies the field.
    pub number: u8,
    /// The length of the field in bytes. This may be a whole multiple of the intrinsic length of
    /// the field's data-type, which indicates field is an array of the base data-type.
    pub length: u8,
    /// A bit-mask identifying the data-type of the field.
    ///
    /// This implementation only looks at the lower 4 bits. The upper 4 bits contain redundant
    /// information.
    pub data_type: u8,
    /// The offset the field begins in the message data.
    ///
    /// This is not present in the file, but is calculatd when parsing each message definition.
    /// For some reason, this is faster even when only a single field is read later on.
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub enum FieldValue {
    //
    I8(i8),
    I8Array(Vec<i8>),
    U8(u8),
    U8Array(Vec<u8>),
    //
    I16(i16),
    I16Array(Vec<i16>),
    U16(u16),
    U16Array(Vec<u16>),
    //
    I32(i32),
    I32Array(Vec<i32>),
    U32(u32),
    U32Array(Vec<u32>),
    //
    I64(i64),
    I64Array(Vec<i64>),
    U64(u64),
    U64Array(Vec<u64>),
    //
    F32(f32),
    F32Array(Vec<f32>),
    F64(f64),
    F64Array(Vec<f64>),
    //
    String(String),
    //
    Nil,
}

/// Defines how an associated data message is formatted.
#[derive(Clone, Debug)]
pub struct MessageDefinition {
    /// Reserved byte, read directly from file structure. Should be zero, not enforced.
    pub reserved: u8,
    /// Global message number.
    pub number: u16,
    /// Total length of corresponding messages in bytes. Does not exist in file structure, but is
    /// stored to save calculating on reading of every message.
    pub length: usize,
    /// The architecture type (little-endian or big-endian) of multi-byte fields in corresponding
    /// messages.
    pub byte_order: Endianness,
    /// Field definitions.
    pub fields: Vec<FieldDefinition>,
    /// Developer field definitions.
    pub developer_fields: Option<Vec<FieldDefinition>>,
}

/// Encapsulates a data message and its associated message definition. The message definition is
/// required for reading the fields in the message.
#[derive(Clone, Debug, PartialEq)]
pub struct Message<'a> {
    /// The definition that corresponds to the message.
    pub definition: Rc<MessageDefinition>,
    /// The unparsed message data.
    pub data: Cow<'a, [u8]>,
}

/// Encapsulates the three high-level data structures in a Fit file, excluding the file header.
#[derive(Debug, PartialEq)]
pub enum Record<'a> {
    Definition(u8, Rc<MessageDefinition>),
    Message(u8, Message<'a>),
}

impl PartialEq for MessageDefinition {
    fn eq(&self, other: &MessageDefinition) -> bool {
        self.reserved == other.reserved &&
        self.number == other.number &&
        self.length == other.length &&
        self.byte_order == other.byte_order &&
        self.fields.iter().zip(other.fields.iter()).all(|(a, b)| a == b) &&
        self.developer_fields.iter().zip(other.developer_fields.iter()).all(|(a, b)| a == b)
    }
}

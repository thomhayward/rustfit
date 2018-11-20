use nom::Endianness;
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

#[derive(Debug, Clone, Copy)]
pub struct FileHeader {
    pub length: u8,
    pub protocol: u8,
    pub profile: u16,
    pub tag: [u8; 4],
    pub file_size: u32,
    pub checksum: Option<u16>,
}

#[derive(Debug, Clone)]
pub struct FieldDefinition {
    /// Field number; identifies the field.
    pub number: u8,
    /// The length of the field in bytes. This may be a whold multiple of the intrinsic length of
    /// the field's data-type, in which case the field is an array of the base data-type.
    pub length: u8,
    /// A bit-mask identifying the data-type of the field. This implementation only looks at the
    /// lower 4 bits. The upper 4 bits contain redundant information.
    pub data_type: u8,
    /// The byte offset at which the field begins in the message data. This is calculatd when
    /// parsing each message definition. For some reason, this is faster even when only a single
    /// field is read later on.
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

#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
pub struct Message<'a> {
    pub definition: Rc<MessageDefinition>,
    pub data: &'a [u8],
}

/// Encapsulates the three high-level data structures in a Fit file, excluding the file header.
#[derive(Debug)]
pub enum Record<'a> {
    Definition(u8, Rc<MessageDefinition>),
    Message(u8, Message<'a>),
    Checksum(u16),
}

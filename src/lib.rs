#[macro_use]
extern crate nom;

mod parser;
pub use self::parser::{
    FieldDefinition, FieldValue, FileHeader, Message, MessageDefinition, Record, RecordHeader,
};

use std::rc::Rc;
use std::slice::Iter;

#[derive(Debug)]
pub enum Error {
    /// The length of the file header is not supported. Should be either 12 or 14 bytes.
    InvalidHeaderSize { found: u8 },
    InvalidHeaderTag { found: Vec<u8>, expected: Vec<u8> },
    HeaderChecksumFailure { found: u16, computed: u16 },
    /// The parser encountered a data message bound to a local-type that does not have an
    /// associated message definition.
    UndefinedLocalType { position: usize, header: u8 },
    /// The parser encountered a definition message that defines a data message longer than 255
    /// bytes.
    InvalidMessageLength { position: usize, header: u8, definition: MessageDefinition },
    EndOfInput,
    InsufficientData,
    Unknown,
}

impl<'a> From<nom::Err<&'a [u8], u32>> for Error {
    fn from(error: nom::Err<&'a [u8], u32>) -> Self {
        use nom::Context::Code;
        use nom::Err::{Error, Incomplete};
        use nom::ErrorKind::Custom;
        match error {
            Error(Code(input, Custom(parser::ERROR_HEADER_SIZE))) => {
                self::Error::InvalidHeaderSize { found: input[0] }
            },
            Error(Code(input, Custom(parser::ERROR_HEADER_TAG))) => {
                self::Error::InvalidHeaderTag {
                    found: input[..4].to_vec(),
                    expected: [0x2e, 0x46, 0x49, 0x54].to_vec(),
                }
            },
            Incomplete(_) => self::Error::InsufficientData,
            _ => self::Error::Unknown,
        }
    }
}

/// FIT Cyclic Redundancy Checksum.
///
/// FIT's handling of checksums is dumb. Generally, it is a very good idea to completely
/// ignore the end of file checksum. The current code only enforces the header checksum.
///
/// # Examples
/// Calculate the CRC of an array of bytes:
/// ```
/// let bytes: [u8; 10] = [43, 23, 23, 71, 95, 21, 38, 90, 91, 32];
/// let checksum = bytes.iter().fold(0, crc);
/// assert_eq!(checksum, 0x4efc);
/// ```
pub fn crc(mut current: u16, byte: &u8) -> u16 {
    const TABLE: [u16; 16] = [
        0x0000, 0xcc01, 0xd801, 0x1400, 0xf001, 0x3c00, 0x2800, 0xe401, 0xa001, 0x6c00, 0x7800,
        0xb401, 0x5000, 0x9c01, 0x8801, 0x4400,
    ];
    let tmp = TABLE[(current & 0x0f) as usize];
    current = current.rotate_right(4) & 0x0fff;
    current = current ^ tmp ^ TABLE[(byte & 0x0f) as usize];
    let tmp = TABLE[(current & 0x0f) as usize];
    current = current.rotate_right(4) & 0x0fff;
    current = current ^ tmp ^ TABLE[(byte.rotate_right(4) & 0x0f) as usize];
    current
}

pub struct Fit<'a> {
    pub header: FileHeader,
    pub data: &'a [u8],
}

pub struct FitParser<'a> {
    data: &'a [u8],
    definitions: [Option<Rc<MessageDefinition>>; 16],
    start_length: usize,
}

pub struct RecordIterator<'a> {
    parser: FitParser<'a>,
}

pub struct MessageIterator<'a> {
    parser: FitParser<'a>,
}

pub struct FieldIterator<'a> {
    message: &'a Message<'a>,
    iterator: Iter<'a, FieldDefinition>,
}

pub struct DeveloperFieldIterator<'a> {
    message: &'a Message<'a>,
    iterator: Iter<'a, FieldDefinition>,
}

impl<'a> Fit<'a> {
    /// Constructs a new Fit object.
    pub fn from_bytes(bytes: &'a [u8]) -> Result<Fit<'a>, Error> {
        let (_, header) = parser::take_file_header(bytes)?;
        if let Some(checksum) = header.checksum {
            let computed = bytes[..12].iter().fold(0, crc);
            if checksum != computed && checksum != 0 {
                return Err(Error::HeaderChecksumFailure {
                    found: checksum,
                    computed
                });
            }
        }
        Ok(Fit {
            header: header,
            data: bytes,
        })
    }
    /// Constructs an iterator over the records (message definitions, messages, and file checksum)
    /// in the Fit file.
    pub fn records(&'a self) -> RecordIterator<'a> {
        RecordIterator {
            parser: FitParser::from(self),
        }
    }
    /// Constructs an iterator over all the messages in the Fit file.
    /// # Example
    /// ```
    /// let power_data = fit
    ///     .messages()
    ///     .filter_map(|ref message| match message.number() {
    ///         20 => message.field_u16(7),
    ///         _ => None
    ///     })
    ///     .collect::<Vec<_>>();
    /// ```
    pub fn messages(&'a self) -> MessageIterator<'a> {
        MessageIterator {
            parser: FitParser::from(self),
        }
    }
    /// Computes the end-of-file checksum.
    pub fn checksum(&self) -> u16 {
        self.data[..(self.data.len() - 2)].iter().fold(0, crc)
    }
    /// Verifies the end of file checksum.
    pub fn verify(&self) -> bool {
        // Slow path ...
        let checksum = self
            .records()
            .filter_map(|record| match record {
                Record::Checksum(value) => Some(value),
                _ => None,
            }).next();
        // Fast path ...
        // let bytes = &self.data[(self.data.len() - 2)..];
        // let checksum = Some((bytes[0] as u16) | ((bytes[1] as u16) << 8));
        match checksum {
            Some(value) => value == self.checksum(),
            None => false,
        }
    }
}

impl<'a> Message<'a> {
    /// Returns the global message number.
    #[inline(always)]
    pub fn number(&self) -> u16 {
        self.definition.number
    }
    /// Returns the value of the field specified by `number`. If the field number is not present,
    /// `None` is returned. A field may present but not set, in which case `FieldValue::Nil` is
    /// returned.
    #[inline(always)]
    pub fn field(&self, number: u8) -> Option<FieldValue> {
        match self.definition.fields.iter().find(|&x| x.number == number) {
            Some(field_definition) => match parser::take_field(self, field_definition) {
                Ok((_, field_value)) => Some(field_value),
                _ => None,
            },
            None => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `i8`.
    #[inline(always)]
    pub fn field_i8(&self, number: u8) -> Option<i8> {
        match self.field(number) {
            Some(FieldValue::I8(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `u8`.
    #[inline(always)]
    pub fn field_u8(&self, number: u8) -> Option<u8> {
        match self.field(number) {
            Some(FieldValue::U8(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `i16`.
    #[inline(always)]
    pub fn field_i16(&self, number: u8) -> Option<i16> {
        match self.field(number) {
            Some(FieldValue::I16(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `u16`.
    #[inline(always)]
    pub fn field_u16(&self, number: u8) -> Option<u16> {
        match self.field(number) {
            Some(FieldValue::U16(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `i32`.
    #[inline(always)]
    pub fn field_i32(&self, number: u8) -> Option<i32> {
        match self.field(number) {
            Some(FieldValue::I32(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is a scalar `u32`.
    #[inline(always)]
    pub fn field_u32(&self, number: u8) -> Option<u32> {
        match self.field(number) {
            Some(FieldValue::U32(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the value of the developer field specified by `number`. If the field number is not
    /// present, `None` is returned.
    #[inline(always)]
    pub fn developer_field(&self, number: u8) -> Option<FieldValue> {
        match &self.definition.developer_fields {
            Some(developer_fields) => match developer_fields.iter().find(|&x| x.number == number) {
                Some(field_definition) => match parser::take_field(self, field_definition) {
                    Ok((_, field_value)) => Some(field_value),
                    _ => None,
                },
                None => None,
            },
            None => None,
        }
    }
    /// Constructs an iterator over all the fields present in the message. Fields are visited in
    /// the order they appear in the raw data.
    pub fn fields(&self) -> FieldIterator {
        FieldIterator {
            message: self,
            iterator: self.definition.fields.iter(),
        }
    }
    /// Constructs an iterator over the developer fields present in the message, if any exist.
    pub fn developer_fields(&self) -> Option<DeveloperFieldIterator> {
        match &self.definition.developer_fields {
            Some(fields) => Some(DeveloperFieldIterator {
                message: self,
                iterator: fields.iter(),
            }),
            None => None,
        }
    }
}

impl<'a> From<&'a Fit<'a>> for FitParser<'a> {
    fn from(fit: &Fit<'a>) -> Self {
        let header_length = fit.header.length as usize;
        FitParser {
            data: &fit.data[header_length..], // skip over the header data
            definitions: [
                None, None, None, None, None, None, None, None, None, None, None, None, None, None,
                None, None,
            ],
            start_length: fit.data.len(),
        }
    }
}

impl<'a> FitParser<'a> {
    #[inline(always)]
    fn step(&mut self) -> Result<parser::Record<'a>, Error> {
        use parser::RecordType;
        if self.data.len() == 2 {
            let (_, checksum) = parser::take_checksum(self.data)?;
            return Ok(Record::Checksum(checksum));
        }
        let (input, header) = parser::take_record_header(self.data)?;
        match header.record_type() {
            RecordType::Data => {
                let ref definition = self.definitions[header.local_type() as usize];
                match definition {
                    Some(def) => {
                        let (input, message) = parser::take_message(input, def)?;
                        self.data = input;
                        Ok(Record::Message(header, message))
                    }
                    None => Err(
                        Error::UndefinedLocalType {
                            position: self.position(),
                            header: header,
                        })
                }
            }
            RecordType::Definition => {
                let (input, definition) = parser::take_message_definition(input, header)?;
                self.data = input;
                match definition.length <= 255 {
                    true => {
                        let def = Rc::new(definition);
                        self.definitions[header.local_type() as usize] = Some(Rc::clone(&def));
                        Ok(Record::Definition(header, Rc::clone(&def)))
                    },
                    false => Err(
                        Error::InvalidMessageLength {
                            position: self.position(),
                            header: header,
                            definition: definition
                        }),
                }
            }
        }
    }

    fn position(&self) -> usize {
        self.data.len() - self.start_length
    }
}

impl<'a> Iterator for RecordIterator<'a> {
    type Item = Record<'a>;
    #[inline(always)]
    fn next(&mut self) -> Option<Record<'a>> {
        match self.parser.step() {
            Ok(record) => Some(record),
            Err(_) => None,
        }
    }
}

impl<'a> Iterator for MessageIterator<'a> {
    type Item = Message<'a>;
    #[inline(always)]
    fn next(&mut self) -> Option<Message<'a>> {
        loop {
            match self.parser.step() {
                Ok(Record::Message(_, message)) => return Some(message),
                Ok(Record::Definition(_, _)) => continue,
                _ => return None,
            }
        }
    }
}

impl<'a> Iterator for FieldIterator<'a> {
    type Item = (FieldDefinition, FieldValue);
    #[inline]
    fn next(&mut self) -> Option<(FieldDefinition, FieldValue)> {
        match self.iterator.next() {
            Some(field_definition) => match self.message.field(field_definition.number) {
                Some(field_value) => Some((field_definition.clone(), field_value)),
                None => None,
            },
            None => None,
        }
    }
}

impl<'a> Iterator for DeveloperFieldIterator<'a> {
    type Item = (FieldDefinition, FieldValue);
    #[inline]
    fn next(&mut self) -> Option<(FieldDefinition, FieldValue)> {
        match self.iterator.next() {
            Some(field_definition) => match self.message.developer_field(field_definition.number) {
                Some(field_value) => Some((field_definition.clone(), field_value)),
                None => None,
            },
            None => None,
        }
    }
}

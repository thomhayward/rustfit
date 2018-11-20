#[macro_use]
extern crate nom;

mod crc;
mod errors;
mod parser;
mod message;

pub use crc::crc;
pub use errors::*;
pub use parser::{
    FieldDefinition, FieldValue, FileHeader, Message, MessageDefinition, Record, RecordHeader,
};
pub use message::*;

use std::rc::Rc;
use std::slice::Iter;

pub trait FitParser<'fit> {
    fn step(&mut self) -> Result<Record<'fit>, Error>;
}

pub trait Fit<'src> {
    type Parser: FitParser<'src>;
    type RecordIterator: Iterator<Item=Record<'src>>;
    type MessageIterator: Iterator<Item=Message<'src>>;
    fn header(&self) -> FileHeader;
    fn records(&'src self) -> Self::RecordIterator;
    fn messages(&'src self) -> Self::MessageIterator;
    fn parser(&'src self) -> Self::Parser;
}

/// Encapsulates a complete Fit file stored in a [u8] slice.
pub struct CompleteFit<'buf> {
    header: FileHeader,
    data: &'buf [u8],
}

pub struct CompleteParser<'a> {
    data: &'a [u8],
    definitions: [Option<Rc<MessageDefinition>>; 16],
}

pub struct CompleteRecordIterator<'a> {
    parser: CompleteParser<'a>,
}

pub struct CompleteMessageIterator<'a> {
    parser: CompleteParser<'a>,
}

impl<'a> CompleteFit<'a> {
    /// Constructs a new Fit object.
    pub fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        let (_, header) = parser::take_file_header(data)?;
        if let Some(checksum) = header.checksum {
            let computed = data[..12].iter().fold(0, crc);
            if checksum != computed && checksum != 0 {
                return Err(Error::HeaderChecksumFailure {
                    found: checksum,
                    computed
                });
            }
        }
        if (header.file_size as usize) < data.len() - header.length as usize - 2 {
            return Err(Error::InsufficientData { required: header.file_size as usize })
        }
        Ok(Self { header, data })
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

impl<'buf> Fit<'buf> for CompleteFit<'buf> {

    type Parser = CompleteParser<'buf>;
    type RecordIterator = CompleteRecordIterator<'buf>;
    type MessageIterator = CompleteMessageIterator<'buf>;

    fn header(&self) -> FileHeader {
        self.header.clone()
    }
    /// Constructs an iterator over the records (message definitions, messages, and file checksum)
    /// in the Fit file.
    fn records(&'buf self) -> Self::RecordIterator {
        CompleteRecordIterator {
            parser: CompleteParser::from(self),
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
    fn messages(&'buf self) -> Self::MessageIterator {
        CompleteMessageIterator {
            parser: CompleteParser::from(self),
        }
    }

    fn parser(&'buf self) -> Self::Parser {
        CompleteParser::from(self)
    }
}

impl<'a> From<&'a CompleteFit<'a>> for CompleteParser<'a> {
    fn from(fit: &CompleteFit<'a>) -> Self {
        let header_length = fit.header.length as usize;
        CompleteParser {
            data: &fit.data[header_length..], // skip over the header data
            definitions: [
                None, None, None, None, None, None, None, None, None, None, None, None, None, None,
                None, None,
            ],
        }
    }
}

impl<'a> FitParser<'a> for CompleteParser<'a> {
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
                            position: 0,
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
                            position: 0,
                            header: header,
                            definition: definition
                        }),
                }
            }
        }
    }
}

impl<'a> Iterator for CompleteRecordIterator<'a> {
    type Item = Record<'a>;
    #[inline(always)]
    fn next(&mut self) -> Option<Record<'a>> {
        match self.parser.step() {
            Ok(record) => Some(record),
            Err(_) => None,
        }
    }
}

impl<'a> Iterator for CompleteMessageIterator<'a> {
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

pub struct ReadParser<'st, R: std::io::Read + 'st> {
    reader: &'st mut R,
    buffer: Vec<u8>,
    parser: CompleteParser<'st>,
}

impl<'a, R: std::io::Read + 'a> FitParser<'a> for ReadParser<'a, R> {
    fn step(&mut self) -> Result<Record<'a>, Error> {
        match self.parser.step() {
            Ok(res) => Ok(res),
            Err(Error::InsufficientData { required }) => {
                let mut buf: Vec<u8> = Vec::with_capacity(required);
                match self.reader.read(&mut buf) {
                    Ok(_read) => {
                        self.buffer.append(&mut buf);
                        return self.step();
                    }
                    Err(_) => {
                        return Err(Error::Unknown);
                    }
                }
            }
            Err(_) => {
                return Err(Error::Unknown);
            }
        }
    }
}

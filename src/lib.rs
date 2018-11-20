#[macro_use]
extern crate nom;

mod crc;
mod errors;
mod parser;
mod message;
mod types;

pub use errors::*;
pub use message::*;
pub use types::*;

/// Encapsulates a complete Fit file stored in a [u8] slice.
pub struct Fit<'buf> {
    pub header: FileHeader,
    pub data: &'buf [u8],
}

pub struct Parser<'a> {
    definitions: [Option<std::rc::Rc<MessageDefinition>>; 16],
    data: &'a [u8],
}

impl<'a> Fit<'a> {
    /// Constructs a new Fit object.
    pub fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        let (_, header) = parser::take_file_header(data)?;
        if let Some(checksum) = header.checksum {
            let computed = data[..12].iter().fold(0, crc::crc);
            if checksum != computed && checksum != 0 {
                return Err(Error::HeaderChecksumFailure {
                    found: checksum,
                    computed
                });
            }
        }
        Ok(Self { header, data })
    }
    /// Computes the end-of-file checksum.
    pub fn checksum(&self) -> u16 {
        self.data[..(self.data.len() - 2)].iter().fold(0, crc::crc)
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

pub struct RecordIterator<'a> {
    parser: Parser<'a>,
}

pub struct MessageIterator<'a> {
    parser: Parser<'a>,
}

impl<'buf> Fit<'buf>{
    /// Constructs an iterator over the records (message definitions, messages, and file checksum)
    /// in the Fit file.
    pub fn records(&'buf self) -> RecordIterator<'buf> {
        RecordIterator {
            parser: Parser::from(self),
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
    pub fn messages(&'buf self) -> MessageIterator<'buf> {
        MessageIterator {
            parser: Parser::from(self),
        }
    }
    pub fn parser(&'buf self) -> Parser<'buf> {
        Parser::from(self)
    }
}

impl<'a> From<&'a Fit<'a>> for Parser<'a> {
    fn from(fit: &Fit<'a>) -> Self {
        let header_length = fit.header.length as usize;
        Parser {
            definitions: [
                None, None, None, None, None, None, None, None, None, None, None, None, None, None,
                None, None,
            ],
            data: &fit.data[header_length..], // skip over the header data
        }
    }
}

impl<'a> Parser<'a> {
    #[inline(always)]
    fn step(&mut self) -> Result<Record<'a>, Error> {
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
                        let def = std::rc::Rc::new(definition);
                        self.definitions[header.local_type() as usize] = Some(std::rc::Rc::clone(&def));
                        Ok(Record::Definition(header, std::rc::Rc::clone(&def)))
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

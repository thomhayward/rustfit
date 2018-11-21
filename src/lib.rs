//! A fast, low-level parser library for Garmin's .FIT format.
//!
//! This parser is lazy!

#[macro_use]
extern crate nom;

mod crc;
mod errors;
mod message;
mod parser;
pub mod types;

pub use crc::crc;
pub use errors::*;
pub use message::*;
pub use types::*;

use std::rc::Rc;

/// Encapsulates a Fit file.
pub struct Fit {
    /// The file header.
    pub header: FileHeader,
    /// The raw file data.
    pub data: Vec<u8>,
    /// The range in `data` which represents the payload.
    payload: std::ops::Range<usize>,
}

/// Manages parser state.
///
/// Usually constructed by one of the [`Fit`] methods: [`parser()`], [`records()`], or [`messages()`]
///
/// [`Fit`]: struct.Fit.html
/// [`parser()`]: struct.Fit.html#method.parser
/// [`records()`]: struct.Fit.html#method.records
/// [`messages()`]: struct.Fit.html#method.messages
pub struct Parser<'data> {
    definitions: [Option<Rc<MessageDefinition>>; 16],
    data: &'data [u8],
}

/// Iterates only the data messages in a Fit file.
pub struct MessageIterator<'data> {
    parser: Parser<'data>,
}

impl<'data> Fit {
    /// Constructs a new Fit object.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    /// Ok(())
    /// }
    /// ```
    ///
    /// This will fail if `data` doesn't begin with a valid header. The following fails with
    /// `Error: InvalidHeaderSize { found: 214 }`.
    /// ```should_panic
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/randombytes.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    /// Ok(())
    /// # }
    /// ```
    pub fn from_bytes(data: &'data [u8]) -> Result<Self, Error> {
        let (_, header) = parser::take_file_header(data)?;
        // If the header checksum is present, verify it.
        if let Some(checksum) = header.checksum {
            let computed = data[..12].iter().fold(0, crc);
            if checksum != computed && checksum != 0 {
                return Err(Error::HeaderChecksumFailure { found: checksum, computed });
            }
        }
        // Check that we have sufficient data.
        let payload = std::ops::Range { start: header.length as usize, end: data.len() - 2 };
        match payload.len() == (header.file_size as usize) {
            true => Ok(Self { header, data: data.to_vec(), payload }),
            false => Err(Error::InsufficientData { required: payload.len() })
        }
        // You may also be tempted to validate the end-of-file checksum at this point. Don't!
    }
    /// Computes the end-of-file [checksum].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    /// assert_eq!(fit.checksum(), 0x0cc8);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [checksum]: fn.crc.html
    pub fn checksum(&self) -> u16 {
        self.data[..self.payload.end].iter().fold(0, crc)
    }
    /// Verifies the end of file [checksum].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    /// assert!(fit.verify_checksum());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [checksum]: fn.crc.html
    pub fn verify_checksum(&self) -> bool {
        let bytes = &self.data[self.payload.end..];
        assert_eq!(bytes.len(), 2, "Last two bytes of data is not 2 bytes long");
        let checksum = (bytes[0] as u16) | ((bytes[1] as u16) << 8);
        checksum == self.checksum()
    }
    /// Constructs a new parser for the Fit file.
    pub fn parser(&'data self) -> Parser<'data> {
        Parser::from(self)
    }
    /// Constructs a [`MessageIterator`] which implements [`Iterator`] over all the
    /// data messages in the Fit file.
    ///
    /// # Examples
    /// Collect all the power data fields for an activity into a `Vec<u16>`:
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    /// let power_data = fit
    ///     .messages()
    ///     .filter_map(|ref message| match message.number() {
    ///         20 => message.field_u16(7),
    ///         _ => None
    ///     })
    ///     .collect::<Vec<_>>();
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [`MessageIterator`]: struct.MessageIterator.html
    /// [`Iterator`]: http://doc.rust-lang.org/std/iter/trait.Iterator.html
    pub fn messages(&'data self) -> MessageIterator<'data> {
        MessageIterator {
            parser: self.parser(),
        }
    }
}

impl<'data> From<&'data Fit> for Parser<'data> {
    fn from(fit: &'data Fit) -> Self {
        Parser {
            definitions: [
                None, None, None, None, None, None, None, None,
                None, None, None, None, None, None, None, None,
            ],
            data: &fit.data[fit.payload.start..fit.payload.end], // skip over the header data
        }
    }
}

impl<'a> Parser<'a> {
    #[inline(always)]
    pub fn step(&mut self) -> Result<Record<'a>, Error> {
        let (input, header) = parser::take_record_header(self.data)?;
        match header.record_type() {
            RecordType::Data => {
                let ref definition = self.definitions[header.local_type() as usize];
                match definition {
                    Some(ref def) => {
                        let (input, message) = parser::take_message_data(input, Rc::clone(def))?;
                        self.data = input;
                        Ok(Record::Message(header, Message {
                            definition: Rc::clone(def),
                            data: message,
                        }))
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

impl<'data> Iterator for Parser<'data> {
    type Item = Record<'data>;
    #[inline(always)]
    fn next(&mut self) -> Option<Record<'data>> {
        match self.step() {
            Ok(record) => Some(record),
            Err(_) => None,
        }
    }
}

impl<'data> Iterator for MessageIterator<'data> {
    type Item = Message<'data>;
    #[inline(always)]
    fn next(&mut self) -> Option<Message<'data>> {
        loop {
            match self.parser.step() {
                Ok(Record::Message(_, message)) => return Some(message),
                Ok(Record::Definition(_, _)) => continue,
                _ => return None,
            }
        }
    }
}

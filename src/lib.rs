//! A fast, low-level parser library for Garmin's .FIT format.
//!
//! ## This parser is lazy!
//! Scanning though a .FIT file with this library is designed to be as cheap as possible. The
//! parser avoids copying data as much as possible. Data messages are returned as slice
//! references, and message fields are only parsed if you attempt to read them.

#[macro_use]
extern crate nom;

mod crc;
mod errors;
mod message;
mod parser;
pub mod timestamp;
pub mod types;

pub use crc::crc;
pub use errors::*;
pub use message::*;
pub use types::*;

use std::borrow::Cow;
use std::rc::Rc;

/// Encapsulates a Fit file.
pub struct Fit<'data> {
    /// FIT file header.
    pub header: FileHeader,
    /// CRC of the header bytes. This is not the same as header.checksum!
    header_checksum: u16,
    /// Payload data (definition records and data records).
    payload: &'data [u8],
    ///
    checksum_bytes: Option<Vec<u8>>,
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
    position: usize,
    data: &'data [u8],
}

/// Iterates only the data messages in a Fit file.
pub struct MessageIterator<'data> {
    parser: Parser<'data>,
}

impl<'data> Fit<'data> {
    /// Initialises a new Fit structure from a byte slice.
    ///
    /// The byte slice must contain a valid Fit header.
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// use rustfit::Fit;
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = Fit::from_bytes(bytes)?;
    ///
    /// assert_eq!(3899, fit.messages().count());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// An incomplete file can be accepted:
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// use rustfit::Fit;
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = Fit::from_bytes(&bytes[..1034])?;
    ///
    /// assert_eq!(11, fit.messages().count());
    /// assert_eq!(false, fit.verify_checksum());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Attempting to load some random bytes will fail with an `Err`:
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// use rustfit::Fit;
    /// let bytes = include_bytes!("../samples/randombytes.fit");
    /// match Fit::from_bytes(bytes) {
    ///     Err(_) => assert!(true),
    ///     Ok(_) => assert!(false),
    /// }
    /// # Ok(())
    /// # }
    /// ```
    ///
    pub fn from_bytes(data: &'data [u8]) -> Result<Self, Error> {

        use nom::error::ErrorKind;

        let (payload, header) = match parser::take_file_header(data) {
            Ok((p, h)) => (p, h),
            Err(nom::Err::Error(nom::error::Error { input, code })) => match code {
                ErrorKind::Verify => return Err(Error::InvalidHeaderSize(input.to_vec())),
                ErrorKind::Tag => return Err(Error::InvalidHeaderTag(input.to_vec())),
                _ => return Err(Error::Unknown)
            },
            Err(_) => return Err(Error::Unknown)
        };

        // If the header checksum is present, verify it.
        if let Some(checksum) = header.checksum {
            let computed = data[..12].iter().fold(0, crc);
            if checksum != computed && checksum != 0 {
                return Err(Error::HeaderChecksumFailure { found: checksum, computed });
            }
        }

        // Calculate the checksum of *all* the bytes in the header. In most cases, this will
        // return 0. This is not the same calculation performed above.
        let header_checksum = 0; //data[..(header.length as usize)].iter().fold(0, crc);

        // Check that we have sufficient data.
        let declared_length = header.file_size as usize;
        let actual_length = payload.len() - 2; // payload contains everything after the header.
        //
        use std::cmp::Ordering;
        match declared_length.cmp(&actual_length) {
            // We've been given either the exact amount of data, or more data than the header
            // thinks we should have.
            Ordering::Less | Ordering::Equal => {
                Ok(Fit {
                    header,
                    header_checksum,
                    payload: &payload[..declared_length],
                    checksum_bytes: Some(payload[declared_length..(declared_length + 2)].to_vec()),
                })
            },
            // We've been given less data than the header claims we should have. The file is
            // incomplete.
            Ordering::Greater => {
                Ok(Fit {
                    header,
                    header_checksum,
                    payload: payload,
                    checksum_bytes: None,
                })
            },
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
    ///
    /// assert_eq!(fit.compute_checksum(), 0x0cc8);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [checksum]: fn.crc.html
    pub fn compute_checksum(&self) -> u16 {
        self.payload.iter().fold(self.header_checksum, crc)
    }
    /// Verifies the end of file [checksum].
    ///
    /// # Examples
    /// ```
    /// # fn main() -> Result<(), rustfit::Error> {
    /// let bytes = include_bytes!("../samples/sample01.fit");
    /// let fit = rustfit::Fit::from_bytes(bytes)?;
    ///
    /// assert!(fit.verify_checksum());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [checksum]: fn.crc.html
    pub fn verify_checksum(&self) -> bool {
        match self.checksum_bytes {
            Some(ref bytes) => {
                match parser::take_checksum(&bytes) {
                    Ok((_, checksum)) => {
                        checksum == self.compute_checksum()
                    },
                    _ => false
                }
            }
            _ => false
        }
    }
    /// Constructs a new parser for the Fit file.
    pub fn parser(&'data self) -> Parser<'data> {
        Parser::from(self.payload)
    }
    /// Returns a [`MessageIterator`] which iterates over all the data messages in the Fit file.
    ///
    /// This is the primary method for extracting data from the Fit file. [`MessageIterator`]
    /// implements [`Iterator`] so it can be used with Rust's `for` loop syntax. Iteration stops
    /// when no more data can be read, or a parsing error occurs.
    ///
    /// If you need to identify parser errors, use [`parser()`].
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
    ///
    /// assert_eq!(3641, power_data.len());
    /// assert_eq!(Some(&1021), power_data.iter().max());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [`MessageIterator`]: struct.MessageIterator.html
    /// [`parser()`]: struct.Fit.html#method.parser
    /// [`Iterator`]: http://doc.rust-lang.org/std/iter/trait.Iterator.html
    pub fn messages(&'data self) -> MessageIterator<'data> {
        MessageIterator {
            parser: self.parser(),
        }
    }
}

impl<'data> From<&'data [u8]> for Parser<'data> {
    /// Initialises a `Parser` from a `u8` slice. `Parser` does not handle the file header,
    /// `from` must only contain the .FIT payload.
    ///
    /// ```
    /// # fn try_main() -> Result<(), rustfit::Error> {
    /// use rustfit::{Parser, Record};
    ///
    /// let bytes: Vec<u8> = vec![0x40, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x2a];
    /// let mut parser = Parser::from(&bytes[..]);
    ///
    /// if let Record::Definition(header, definition) = parser.step()? {
    ///     assert_eq!(0x40, header);
    ///     assert_eq!(1, definition.length);
    /// }
    ///
    /// if let Record::Message(_, message) = parser.step()? {
    ///     assert_eq!(Some(42), message.field_u8(0));
    ///     assert_eq!(1, message.fields().count());
    /// }
    ///
    /// # Ok(())
    /// # }
    /// ```
    fn from(from: &'data [u8]) -> Self {
        Parser {
            definitions: [
                None, None, None, None, None, None, None, None,
                None, None, None, None, None, None, None, None,
            ],
            position: 0,
            data: from,
        }
    }
}

impl<'a> Parser<'a> {
    #[inline(always)]
    pub fn step(&mut self) -> Result<Record<'a>, Error> {
        let remaining = self.data.len();
        if remaining == 0 {
            return Err(Error::EndOfInput);
        }
        let (input, header) = match parser::take_record_header(self.data) {
            Ok((i, h)) => (i, h),
            Err(_) => return Err(Error::Unknown)
        };
        match header.record_type() {
            RecordType::Data => {
                let ref definition = self.definitions[header.local_type() as usize];
                match definition {
                    Some(ref def) => {
                        let (input, message) = match parser::take_message_data(def.length)(input) {
                            Ok((i, m)) => (i, m),
                            Err(_) => return Err(Error::Unknown)
                        };
                        self.data = input;
                        self.position += remaining - input.len();
                        Ok(Record::Message(header, Message {
                            definition: Rc::clone(def),
                            data: Cow::Borrowed(message),
                        }))
                    }
                    None => Err(
                        Error::UndefinedLocalType {
                            position: self.position,
                            header: header,
                        })
                }
            }
            RecordType::Definition => {
                let (input, definition) = match parser::take_message_definition(header)(input) {
                    Ok((i, d)) => (i, d),
                    Err(_) => return Err(Error::Unknown)
                };
                self.data = input;
                match definition.length <= 255 {
                    true => {
                        let def = Rc::new(definition);
                        self.definitions[header.local_type() as usize] = Some(Rc::clone(&def));
                        self.position += remaining - input.len();
                        Ok(Record::Definition(header, Rc::clone(&def)))
                    },
                    false => Err(
                        Error::InvalidMessageLength {
                            position: self.position,
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
    fn next(&mut self) -> Option<Self::Item> {
        match self.step() {
            Ok(record) => Some(record),
            Err(_) => None,
        }
    }
}

impl<'data> Iterator for MessageIterator<'data> {
    type Item = Message<'data>;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.parser.step() {
                Ok(Record::Message(_, message)) => return Some(message),
                Ok(Record::Definition(_, _)) => continue,
                Err(_) => return None,
            }
        }
    }
}

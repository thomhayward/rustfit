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
    /// assert_eq!(133688, fit.data.len());
    /// # Ok(())
    /// # }
    /// ```
    ///
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
    ///
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
    ///
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

impl Default for Fit {
    /// Initialises an empty `Fit` structure.
    ///
    /// Calling [`checksum()`], or [`verify_checksum()`] on the result will not panic, and
    /// [`parser()`], or [`messages()`] will produce Iterators that produce no items.
    ///
    /// ```
    /// use rustfit::{Fit, FileHeader, Error};
    ///
    /// let fit = Fit::default();
    /// assert_eq!(fit.header, FileHeader::default());
    /// assert_eq!(fit.checksum(), 0x0000);
    /// assert_eq!(fit.verify_checksum(), false);
    ///
    /// let mut parser = fit.parser();
    /// assert_eq!(1, match parser.step() {
    ///     Err(Error::InsufficientData { required }) => required,
    ///     _ => usize::max_value(),
    /// });
    ///
    /// assert_eq!(None, parser.next());
    /// assert_eq!(None, parser.next());
    ///
    /// let mut messages = fit.messages();
    /// assert_eq!(None, messages.next());
    /// assert_eq!(None, messages.next());
    /// assert_eq!(None, messages.next());
    /// ```
    ///
    /// [`checksum()`]: struct.Fit.html#method.checksum
    /// [`verify_checksum()`]: struct.Fit.html#method.verify_checksum
    /// [`parser()`]: struct.Fit.html#method.parser
    /// [`messages()`]: struct.Fit.html#method.messages
    fn default() -> Self {
        Fit {
            header: FileHeader::default(),
            data: vec![255, 255],
            payload: (0..0),
        }
    }
}

/// Creates a [`Fit`] from any type that implements [`Read`]
///
/// # Examples
/// ```
/// # fn main() -> Result<(), rustfit::Error> {
/// use std::path::Path;
/// use std::fs::File;
/// let path = Path::new("samples/sample01.fit");
/// let mut file = File::open(&path).expect("Error opening file");
/// let fit = rustfit::Fit::from(&mut file);
///
/// assert!(fit.verify_checksum());
/// # Ok(())
/// # }
/// ```
///
/// On failure, a default [`Fit`] structure is returned:
/// ```
/// # fn main() -> Result<(), rustfit::Error> {
/// use std::path::Path;
/// use std::fs::File;
/// let path = Path::new("samples/randombytes.fit");
/// let mut file = File::open(&path).expect("Error opening file");
/// let fit = rustfit::Fit::from(&mut file);
///
/// assert_ne!(fit.verify_checksum(), true, "Checksum should not be valid!");
/// assert_eq!(fit.header.length, 0);
/// # Ok(())
/// # }
/// ```
///
/// [`Fit`]: struct.Fit.html
/// [`Read`]: http://doc.rust-lang.org/std/os/trait.Read.html
impl<'a, T> From<&'a mut T> for Fit where T: std::io::Read {
    fn from(from: &'a mut T) -> Self {
        let mut buffer: Vec<u8> = Vec::new();
        match from.read_to_end(&mut buffer) {
            Ok(_) => {
                match Fit::from_bytes(&buffer) {
                    Ok(fit) => fit,
                    Err(_) => Fit::default()
                }
            }
            Err(_) => Fit::default()
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
                _ => return None,
            }
        }
    }
}

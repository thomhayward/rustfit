use super::*;

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
    InsufficientData { required: usize },

    Incomplete { expected: usize, have: usize },
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
            Incomplete(nom::Needed::Size(size)) => self::Error::InsufficientData {
                required: size as usize
            },
            _ => self::Error::Unknown,
        }
    }
}

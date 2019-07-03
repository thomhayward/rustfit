use nom::number::Endianness;
use nom::number::streaming::{be_f32, be_f64, le_f32, le_f64, le_i8, le_u16, le_u32, le_u8};
use nom::IResult;
use nom::ToUsize;
use std::borrow::Borrow;
use super::types::*;

macro_rules! f32 ( ($i:expr, $e:expr) => ( {if Endianness::Big == $e { be_f32($i) } else { le_f32($i) } } ););
macro_rules! f64 ( ($i:expr, $e:expr) => ( {if Endianness::Big == $e { be_f64($i) } else { le_f64($i) } } ););

/// Consumes a file header.
///
/// Will reject under the following conditions:
///   - Field header length does not equal 12 or 14 bytes
///   - File tag does not equal '.FIT'
///
/// The header checksum is not verified.
///
pub fn take_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    use nom::combinator::{cond, verify};
    use nom::bytes::streaming::tag;
    let (input, length) = verify(le_u8, |&val: &u8| val == 12 || val == 14)(input)?;
    let (input, protocol) = le_u8(input)?;
    let (input, profile) = le_u16(input)?;
    let (input, file_size) = le_u32(input)?;
    let (input, fit_tag) = tag(".FIT")(input)?;
    let (input, checksum) = cond(length == 14, le_u16)(input)?;
    Ok((input, FileHeader {
        length, protocol, profile, tag: [fit_tag[0], fit_tag[1], fit_tag[2], fit_tag[3]],
        file_size, checksum
    }))
}

#[inline(always)]
pub fn take_record_header(input: &[u8]) -> IResult<&[u8], u8> {
    le_u8(input)
}

#[inline(always)]
pub fn take_field_definition(input: &[u8]) -> IResult<&[u8], (u8, u8, u8)> {
    // Field Definition: (field_number: u8, length: u8, data_type: u8)
    // - field_number should not equal 255
    use nom::combinator::verify;
    use nom::sequence::tuple;
    tuple((verify(le_u8, |&fnum: &u8| fnum != 255), le_u8, le_u8))(input)
}

#[inline]
fn take_field_definitions(input: &[u8]) -> IResult<&[u8], Vec<(u8, u8, u8)>> {
    use nom::combinator::verify;
    use nom::multi::count;
    let (input, cnt) = verify(le_u8, |&c :&u8| c > 0)(input)?;
    count(take_field_definition, cnt.to_usize())(input)
}

#[inline(always)]
pub fn take_byteorder(input: &[u8]) -> IResult<&[u8], Endianness> {
    use nom::combinator::verify;
    match verify(le_u8, |&val: &u8| val == 0 || val == 1)(input)? {
        (i, 0) => Ok((i, Endianness::Little)),
        (i, _) => Ok((i, Endianness::Big))
    }
}

/// Takes a slice of raw field definitions, and calculates the offset at which each field will
/// occur in corresponding messages.
///
pub fn process_field_definitions(
    raw_fields: &[(u8, u8, u8)],
    start_offset: usize,
) -> Vec<FieldDefinition> {
    raw_fields
        .iter()
        .scan(start_offset, |offset, &(number, length, data_type)| {
            let current_offset = *offset;
            *offset += length.to_usize();
            Some(FieldDefinition {
                number: number,
                length: length.to_usize(),
                offset: current_offset,
                data_type: data_type,
            })
        }).collect::<Vec<_>>()
}

#[inline]
pub fn take_message_definition(header: u8) -> impl Fn(&[u8]) -> IResult<&[u8], MessageDefinition> {
    move |input: &[u8]| {
        use nom::combinator::cond;
        use nom::sequence::tuple;
        let (input, (reserved, byte_order)) = tuple((le_u8, take_byteorder))(input)?;
        let (input, number) = take_u16(byte_order)(input)?;
        let (input, (fields, developer_fields)) = tuple((take_field_definitions, cond(header.developer(), take_field_definitions)))(input)?;
        //
        let base_field_length = fields.iter().fold(0, |a, &(_, length, _)| a + (length.to_usize()));
        let devl_field_length = match &developer_fields {
            Some(fields) => fields.iter().fold(0, |a, &(_, length, _)| a + (length.to_usize())),
            None => 0
        };
        Ok((input, MessageDefinition {
            reserved,
            number,
            length: base_field_length + devl_field_length,
            byte_order,
            fields: process_field_definitions(&fields, 0),
            developer_fields: match &developer_fields {
                Some(fields) => Some(process_field_definitions(&fields, base_field_length)),
                None => None
            }
        }))
    }
}

#[inline(always)]
pub fn take_message_data(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
    move |input: &[u8]| {
        nom::bytes::streaming::take(length)(input)
    }
}

#[inline(always)]
pub fn take_checksum(input: &[u8]) -> IResult<&[u8], u16> {
    le_u16(input)
}

#[inline(always)]
pub fn take_u16(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], u16> {
    move |input: &[u8]| {
        u16!(input, endianness)
    }
}

#[inline(always)]
pub fn take_i16(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], i16> {
    move |input: &[u8]| {
        i16!(input, endianness)
    }
}

#[inline(always)]
pub fn take_u32(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], u32> {
    move |input: &[u8]| {
        u32!(input, endianness)
    }
}

#[inline(always)]
pub fn take_i32(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], i32> {
    move |input: &[u8]| {
        i32!(input, endianness)
    }
}

#[inline(always)]
pub fn take_u64(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], u64> {
    move |input: &[u8]| {
        u64!(input, endianness)
    }
}

#[inline(always)]
pub fn take_i64(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], i64> {
    move |input: &[u8]| {
        i64!(input, endianness)
    }
}

/// Returns a field from `message` using the supplied field definition. Note that the whether
/// `field_definition` is valid for the message is not checked.
pub fn take_field<'a>(field_definition: &'a FieldDefinition) -> impl Fn(&'a Message) -> IResult<&'a [u8], FieldValue> {
    use nom::bytes::streaming::take;
    use nom::multi::count;
    move |message: &'a Message| {
        let endianness = message.definition.byte_order;
        let data: &[u8] = message.data.borrow();
        let (input, _) = take(field_definition.offset)(data)?;
        match field_definition.data_type & 0x0f {
            0 | 2 | 13 => {
                if field_definition.length == 1 {
                    match le_u8(input)? {
                        (i, std::u8::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U8(value))),
                    }
                } else {
                    let (i, bytes) = take(field_definition.length)(input)?;
                    // TODO: Ascertain whether this is correct
                    match bytes.iter().all(|&v| v == std::u8::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U8Array(bytes.to_vec()))),
                    }
                }
            }
            1 => {
                if field_definition.length == 1 {
                    match le_i8(input)? {
                        (i, std::i8::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::I8(value))),
                    }
                } else {
                    let value_count = field_definition.length;
                    let (i, values) = count(le_i8, value_count)(input)?;
                    match values.iter().all(|&v| v == std::i8::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::I8Array(values.to_vec()))),
                    }
                }
            }
            3 => {
                if field_definition.length == std::mem::size_of::<i16>() {
                    match take_i16(endianness)(input)? {
                        (i, std::i16::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::I16(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<i16>();
                    let (i, values) = count(take_i16(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::i16::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::I16Array(values.to_vec()))),
                    }
                }
            }
            4 => {
                if field_definition.length == std::mem::size_of::<u16>() {
                    match take_u16(endianness)(input)? {
                        (i, std::u16::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U16(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u16>();
                    let (i, values) = count(take_u16(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u16::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U16Array(values.to_vec()))),
                    }
                }
            }
            5 => {
                if field_definition.length == std::mem::size_of::<i32>() {
                    match take_i32(endianness)(input)? {
                        (i, std::i32::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::I32(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<i32>();
                    let (i, values) = count(take_i32(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::i32::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::I32Array(values.to_vec()))),
                    }
                }
            }
            6 => {
                if field_definition.length == std::mem::size_of::<u32>() {
                    match take_u32(endianness)(input)? {
                        (i, std::u32::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U32(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u32>();
                    let (i, values) = count(take_u32(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u32::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U32Array(values.to_vec()))),
                    }
                }
            }
            7 => {
                // null terminated, utf8 string
                let (_, bytes) = take(field_definition.length)(input)?;
                match bytes.iter().position(|&v| v == 0) {
                    Some(0) => Ok((input, FieldValue::Nil)),
                    Some(x) => {
                        let (chopped, _) = bytes.split_at(x);
                        match String::from_utf8(chopped.to_vec()) {
                            Ok(string) => Ok((input, FieldValue::String(string))),
                            // TODO: Fix me!
                            Err(_) => Ok((input, FieldValue::String("Hi".to_string())))
                            //Err(_) => Err(nom::Err::Error(&bytes, ERROR_UTF8_ERROR)),
                        }
                    }
                    None => match String::from_utf8(bytes.to_vec()) {
                        Ok(string) => Ok((input, FieldValue::String(string))),
                        // TODO: Fix me!
                        Err(_) => Ok((input, FieldValue::String("Hi".to_string())))
                        //Err(_) => Err(Failure(Code(&bytes, Custom(ERROR_UTF8_ERROR)))),
                    },
                }
            }
            8 => {
                // 32-bit Float
                let (_, bytes) = take(field_definition.length)(input)?;
                if bytes.iter().all(|&v| v == std::u8::MAX) {
                    return Ok((input, FieldValue::Nil));
                }
                if field_definition.length == std::mem::size_of::<f32>() {
                    let (i, value) = f32!(input, endianness)?;
                    Ok((i, FieldValue::F32(value)))
                } else {
                    let count = field_definition.length / std::mem::size_of::<f32>();
                    let (i, values) = do_parse!(input, vs: count!(f32!(endianness), count) >> (vs))?;
                    Ok((i, FieldValue::F32Array(values.to_vec())))
                }
            }
            9 => {
                // 64-bit Float
                let (_, bytes) = take(field_definition.length)(input)?;
                if bytes.iter().all(|&v| v == std::u8::MAX) {
                    return Ok((input, FieldValue::Nil));
                }
                if field_definition.length == std::mem::size_of::<f64>() {
                    let (i, value) = f64!(input, endianness)?;
                    Ok((i, FieldValue::F64(value)))
                } else {
                    let count = field_definition.length / std::mem::size_of::<f64>();
                    let (i, values) = do_parse!(input, vs: count!(f64!(endianness), count) >> (vs))?;
                    Ok((i, FieldValue::F64Array(values.to_vec())))
                }
            }
            10 => {
                // Unsigned, non-zero 8-bit integer
                if field_definition.length == 1 {
                    match le_u8(input)? {
                        (i, std::u8::MIN) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U8(value))),
                    }
                } else {
                    let (i, bytes) = take(field_definition.length)(input)?;
                    match bytes.iter().all(|&v| v == std::u8::MIN) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U8Array(bytes.to_vec()))),
                    }
                }
            }
            11 => {
                // Unsigned, non-zero 16-bit integer
                if field_definition.length == std::mem::size_of::<u16>() {
                    match take_u16(endianness)(input)? {
                        (i, std::u16::MIN) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U16(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u16>();
                    let (i, values) = count(take_u16(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u16::MIN) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U16Array(values.to_vec()))),
                    }
                }
            }
            12 => {
                // Unsigned, non-zero 32-bit integer
                if field_definition.length == std::mem::size_of::<u32>() {
                    match take_u32(endianness)(input)? {
                        (i, std::u32::MIN) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U32(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u32>();
                    let (i, values) = count(take_u32(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u32::MIN) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U32Array(values.to_vec()))),
                    }
                }
            }
            14 => {
                if field_definition.length == std::mem::size_of::<i64>() {
                    match take_i64(endianness)(input)? {
                        (i, std::i64::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::I64(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<i64>();
                    let (i, values) = count(take_i64(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::i64::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::I64Array(values.to_vec()))),
                    }
                }
            }
            15 => {
                if field_definition.length == std::mem::size_of::<u64>() {
                    match take_u64(endianness)(input)? {
                        (i, std::u64::MAX) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U64(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u64>();
                    let (i, values) = count(take_u64(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u64::MAX) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U64Array(values.to_vec()))),
                    }
                }
            }
            16 => {
                if field_definition.length == std::mem::size_of::<u64>() {
                    match take_u64(endianness)(input)? {
                        (i, std::u64::MIN) => Ok((i, FieldValue::Nil)),
                        (i, value) => Ok((i, FieldValue::U64(value))),
                    }
                } else {
                    let value_count = field_definition.length / std::mem::size_of::<u64>();
                    let (i, values) = count(take_u64(endianness), value_count)(input)?;
                    match values.iter().all(|&v| v == std::u64::MIN) {
                        true => Ok((i, FieldValue::Nil)),
                        false => Ok((i, FieldValue::U64Array(values.to_vec()))),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

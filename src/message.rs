//! Methods for accessing message data.
//!
use super::*;

/// Iterates over the standard fields in a message.
pub struct FieldIterator<'a> {
    message: &'a Message<'a>,
    iterator: std::slice::Iter<'a, FieldDefinition>,
}

/// Iterates over the developer fields in a message.
pub struct DeveloperFieldIterator<'a> {
    message: &'a Message<'a>,
    iterator: std::slice::Iter<'a, FieldDefinition>,
}

/// Implements field accessors for [`Message`].
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
            Some(field_definition) => match parser::take_field(field_definition)(self) {
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
    /// Returns the field specified by `number` iff the field can be represented as a scalar `i16`.
    #[inline(always)]
    pub fn field_i16(&self, number: u8) -> Option<i16> {
        match self.field(number) {
            Some(FieldValue::I8(value)) => Some(value.into()),
            Some(FieldValue::I16(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field can be represented as a scalar `u16`.
    #[inline(always)]
    pub fn field_u16(&self, number: u8) -> Option<u16> {
        match self.field(number) {
            Some(FieldValue::U8(value)) => Some(value.into()),
            Some(FieldValue::U16(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field is can be represented as scalar `i32`.
    #[inline(always)]
    pub fn field_i32(&self, number: u8) -> Option<i32> {
        match self.field(number) {
            Some(FieldValue::I8(value)) => Some(value.into()),
            Some(FieldValue::I16(value)) => Some(value.into()),
            Some(FieldValue::I32(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field can be represented as a scalar `u32`.
    #[inline(always)]
    pub fn field_u32(&self, number: u8) -> Option<u32> {
        match self.field(number) {
            Some(FieldValue::U8(value)) => Some(value.into()),
            Some(FieldValue::U16(value)) => Some(value.into()),
            Some(FieldValue::U32(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field can be represented as a scalar `i64`.
    #[inline(always)]
    pub fn field_i64(&self, number: u8) -> Option<i64> {
        match self.field(number) {
            Some(FieldValue::I8(value)) => Some(value.into()),
            Some(FieldValue::I16(value)) => Some(value.into()),
            Some(FieldValue::I32(value)) => Some(value.into()),
            Some(FieldValue::I64(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the field specified by `number` iff the field can be represented as a scalar `u64`.
    #[inline(always)]
    pub fn field_u64(&self, number: u8) -> Option<u64> {
        match self.field(number) {
            Some(FieldValue::U8(value)) => Some(value.into()),
            Some(FieldValue::U16(value)) => Some(value.into()),
            Some(FieldValue::U32(value)) => Some(value.into()),
            Some(FieldValue::U64(value)) => Some(value),
            _ => None,
        }
    }
    /// Returns the value of the developer field specified by `number`. If the field number is not
    /// present, `None` is returned.
    #[inline(always)]
    pub fn developer_field(&self, number: u8) -> Option<FieldValue> {
        match &self.definition.developer_fields {
            Some(developer_fields) => match developer_fields.iter().find(|&x| x.number == number) {
                Some(field_definition) => match parser::take_field(field_definition)(self) {
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

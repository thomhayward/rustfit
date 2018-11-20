/// Computes Fit's CRC.
///
/// # Examples
/// Calculate the CRC of some bytes:
/// ```
/// let bytes: [u8; 10] = [43, 23, 23, 71, 95, 21, 38, 90, 91, 32];
/// let checksum = bytes.iter().fold(0, rustfit::crc);
/// assert_eq!(checksum, 0x4efc);
/// ```
#[inline]
pub fn crc(mut current: u16, byte: &u8) -> u16 {
    const TABLE: [u16; 16] = [
        0x0000, 0xcc01, 0xd801, 0x1400, 0xf001, 0x3c00, 0x2800, 0xe401,
        0xa001, 0x6c00, 0x7800, 0xb401, 0x5000, 0x9c01, 0x8801, 0x4400,
    ];
    let tmp = TABLE[(current & 0x0f) as usize];
    current = current.rotate_right(4) & 0x0fff;
    current = current ^ tmp ^ TABLE[(byte & 0x0f) as usize];
    let tmp = TABLE[(current & 0x0f) as usize];
    current = current.rotate_right(4) & 0x0fff;
    current = current ^ tmp ^ TABLE[(byte.rotate_right(4) & 0x0f) as usize];
    current
}

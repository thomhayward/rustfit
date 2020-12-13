/// Number of seconds between UNIX Epoch and FIT Epoch.
///
/// Add this value to any FIT timestamp to get the equivalent UNIX timestamp.
///
pub const TIMESTAMP_OFFSET: u32 = 631065600;

pub fn timestamp_fit_to_unix(fit_ts: u32) -> Option<u32> {
    fit_ts.checked_add(TIMESTAMP_OFFSET)
}

pub fn timestamp_unix_to_fit(unix_ts: u32) -> Option<u32> {
    unix_ts.checked_sub(TIMESTAMP_OFFSET)
}

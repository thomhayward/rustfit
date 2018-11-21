extern crate rustfit;

use rustfit::Fit;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn calculate_mean_power(fit: &Fit) -> Option<f32> {
    // 1. Iterate over #20 (record) messages, and extract any power values (field #7)
    let values = fit
        .messages()
        .filter_map(|ref message| match message.number() {
            20 => message.field_u16(7),
            _ => None
        });
    // 2. Calculate the mean, avoiding use of `.collect()` (because memory allocations are slow)
    let (count, sum) = values.fold((0u16, 0u32), |(c, total), v| (c + 1, total + v as u32));
    // 3. `count` will be 0 iff there is no power data in the file
    match count {
        0 => None,
        _ => Some(sum as f32 / count as f32),
    }
}

#[derive(Debug)]
enum Error {
    IoError(std::io::Error),
    RustFitError(rustfit::Error),
}
impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IoError(error)
    }
}
impl From<rustfit::Error> for Error {
    fn from(error: rustfit::Error) -> Self {
        Error::RustFitError(error)
    }
}

fn main() -> Result<(), Error> {
    let mut args = env::args();
    args.next();

    for argument in args {
        let path = Path::new(&argument);
        let filename = path.file_name().unwrap_or_else(|| path.as_os_str()).to_string_lossy();
        let mut file = File::open(&path)?;
        let mut buf: Vec<u8> = Vec::new();
        file.read_to_end(&mut buf)?;
        match Fit::from_bytes(&buf) {
            Ok(fit) => {
                match calculate_mean_power(&fit) {
                    Some(mean) => println!("{:?}: mean power = {:.0} Watts", filename, mean),
                    None => println!("{:?}: no power data", filename),
                }
            },
            Err(error) => {
                eprintln!("{:?}", error);
            }
        }
    }
    Ok(())
}

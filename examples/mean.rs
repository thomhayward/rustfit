extern crate rustfit;

use rustfit::Fit;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn calculate_mean_power(fit: &Fit) -> Option<f64> {
    let (count, sum) = fit
        .messages()
        .filter_map(|message| match message.number() {
            20 => message.field_u32(7),
            _ => None,
        })
        .fold((0_u32, 0_f64), |(count, sum), value| {
            (count + 1, sum + f64::from(value))
        });
    // `count` will be 0 iff there is no power data in the file
    match count {
        0 => None,
        _ => Some(sum / f64::from(count)),
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
        let filename = path
            .file_name()
            .unwrap_or(path.as_os_str())
            .to_string_lossy();
        let mut file = File::open(&path)?;
        let mut buf: Vec<u8> = Vec::with_capacity(1e6 as usize);
        file.read_to_end(&mut buf)?;
        match Fit::from_bytes(&buf) {
            Ok(fit) => match calculate_mean_power(&fit) {
                Some(mean) => println!("{}: mean power = {:.0} Watts", filename, mean),
                None => println!("{}: no power data", filename),
            },
            Err(error) => {
                eprintln!("{:?}", error);
            }
        }
    }
    Ok(())
}

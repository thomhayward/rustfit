extern crate rayon;
extern crate rustfit;

use rayon::prelude::*;
use rustfit::Fit;
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

fn load(filename: &String) -> Option<Vec<u8>> {
    const BUFFER_LENGTH: usize = 512e3 as usize;
    let path = Path::new(&filename);
    match File::open(&path) {
        Ok(mut file) => {
            let mut buffer: Vec<u8> = Vec::with_capacity(BUFFER_LENGTH);
            match file.read_to_end(&mut buffer) {
                Ok(_) => Some(buffer),
                Err(error) => {
                    eprintln!("{:?}", error);
                    None
                }
            }
        },
        Err(error) => {
            eprintln!("{:?}", error);
            None
        }
    }
}

fn main() {
    let start = std::time::Instant::now();
    let mut args = std::env::args();
    args.next();
    let files = args.collect::<Vec<_>>();
    {
        files.par_iter().for_each(|filename| {
            if let Some(data) = load(filename) {
                match Fit::from_bytes(&data) {
                    Ok(fit) => match calculate_mean_power(&fit) {
                        Some(mean) => println!("{}: mean power = {:.0} Watts", filename, mean),
                        None => println!("{}: no power data", filename),
                    },
                    Err(error) => eprintln!("{:?}", error),
                }
            }
        });
    }
    eprintln!("{:?}", start.elapsed());
}

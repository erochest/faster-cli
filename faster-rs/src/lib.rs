#![feature(entry_and_modify)]
#![feature(slice_rsplit)]

use std::collections::hash_map::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::iter::Iterator;

pub fn run(filename: String, key_index: usize, value_index: usize) {
    let f = File::open(filename).unwrap();
    let mut reader = BufReader::new(f);
    let mut buffer = Vec::with_capacity(256);
    let mut counts: HashMap<usize, usize> = HashMap::new();

    loop {
        buffer.clear();

        let read = reader
            .read_until(b'\n', &mut buffer)
            .expect("Unable to read from input.");
        if read == 0 {
            break;
        }

        if let Some(pair) = parse_line(&buffer, key_index, value_index) {
            count_items(&mut counts, pair);
        }
    }

    counts
        .into_iter()
        .max_by_key(|p| p.1)
        .map(|(k, v)| println!("max-key: {}\tsum: {}", k, v));
}

fn parse_line(line: &[u8], key_index: usize, value_index: usize) -> Option<(usize, usize)> {
    let mut words = line.split(|n| n.is_ascii_whitespace());
    let key = parse_nth(&mut words, key_index)?;
    let value = parse_nth(&mut words, value_index - (key_index + 1))?;
    Some((key, value))
}

fn parse_nth<'a, I>(input: &mut I, n: usize) -> Option<usize>
where
    I: Iterator<Item = &'a [u8]>,
{
    input.nth(n).and_then(parse_bytes)
}

fn read_next_field(input: &[u8]) -> Option<(&[u8], usize)> {
    let mut field = Vec::with_capacity(32);
    for (i, c) in input.iter().enumerate() {
        if *c == b'\t' {
            return Some((field.as_slice(), i + 1));
        }
        field.push(*c);
    }
    None
}

fn parse_bytes(input: &[u8]) -> Option<usize> {
    // 0 1 2 3 4 5 6 7 8 9
    let mut n: usize = 0;
    let mut read: usize = 0;

    for c in input {
        if *c < 48 || *c > 57 {
            break;
        }
        n = (n * 10) + usize::from(*c - 48);
        read += 1;
    }

    if read == 0 {
        None
    } else {
        Some(n)
    }
}

fn count_items(freqs: &mut HashMap<usize, usize>, pair: (usize, usize)) {
    let (k, v) = pair;
    freqs.entry(k).and_modify(|e| *e += v).or_insert(v);
}

#[cfg(test)]
mod test;

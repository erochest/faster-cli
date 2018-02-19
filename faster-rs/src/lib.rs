#![feature(entry_and_modify)]

use std::collections::hash_map::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::Iterator;
use std::str::FromStr;

const BUFFER_SIZE: usize = 12 * 1024;

pub fn run(filename: String, key_index: usize, value_index: usize) {
    let f = File::open(filename).unwrap();
    let reader = BufReader::with_capacity(BUFFER_SIZE, f);

    reader
        .lines()
        .into_iter()
        .filter_map(|line| parse_line(line, key_index, value_index))
        .fold(HashMap::new(), count_items)
        .into_iter()
        .max_by_key(|p| p.1)
        .into_iter()
        .for_each(|(k, v)| println!("max-key: {}\tsum: {}", k, v));
}

fn parse_line(
    line: io::Result<String>,
    key_index: usize,
    value_index: usize,
) -> Option<(usize, usize)> {
    let line = line.ok()?;
    let mut words = line.split_whitespace();
    let key = parse_nth(&mut words, key_index)?;
    let value = parse_nth(&mut words, value_index - (key_index + 1))?;
    Some((key, value))
}

fn parse_nth<I, O>(input: &mut I, n: usize) -> Option<O>
where
    I: Iterator,
    I::Item: ToString,
    O: FromStr,
{
    input.nth(n).and_then(|v| v.to_string().parse().ok())
}

fn count_items(mut freqs: HashMap<usize, usize>, pair: (usize, usize)) -> HashMap<usize, usize> {
    let (k, v) = pair;
    freqs.entry(k).and_modify(|e| *e += v).or_insert(v);
    freqs
}

#[cfg(test)]
mod test;

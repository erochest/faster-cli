#![feature(slice_patterns)]
#![feature(entry_and_modify)]

use std::collections::hash_map::HashMap;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn main() {
    let args = env::args();
    if args.len() < 4 {
        panic!("usage: faster-rs FILENAME KEY-INDEX VALUE-INDEX");
    }
    let args = args.skip(1).take(3).collect::<Vec<String>>();
    let filename = args[0].clone();
    let key_index = args[1].parse::<usize>().unwrap();
    let value_index = args[2].parse::<usize>().unwrap();

    let f = File::open(filename).unwrap();
    let reader = BufReader::new(f);

    reader
        .lines()
        .into_iter()
        .filter_map(|line| {
            let line = line.unwrap();
            let mut words = line.split_whitespace();
            words.nth(key_index)
                .and_then(|k| k.parse::<usize>().ok())
                .and_then(|k| {
                    words
                        .nth(value_index - key_index)
                        .and_then(|v| v.parse::<usize>().ok())
                        .map(|v| (k, v))
                })
        })
        .fold(HashMap::new(), |mut index, (k, v)| {
            index.entry(k).and_modify(|e| *e += v).or_insert(v);
            index
        })
        .into_iter()
        .max_by_key(|p| p.1)
        .into_iter()
        .for_each(|(k, v)| println!("max-key: {}\tsum: {}", k, v));
}

extern crate faster_rs;

use std::env;

fn main() {
    let args = env::args();
    if args.len() < 4 {
        panic!("usage: faster-rs FILENAME KEY-INDEX VALUE-INDEX");
    }
    let args = args.skip(1).take(3).collect::<Vec<String>>();
    let filename = args[0].clone();
    let key_index = args[1].parse::<usize>().unwrap();
    let value_index = args[2].parse::<usize>().unwrap();

    faster_rs::run(filename, key_index, value_index);
}

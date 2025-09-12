//! This example is a baseline for size comparison,
//! to count for `std::env::args_os`'s cost.
use std::hint::black_box;

fn main() {
    let args = std::env::args_os().collect::<Vec<_>>();
    println!("hello world");
    black_box(args);
}

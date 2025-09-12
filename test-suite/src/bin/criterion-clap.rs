use clap::{Parser, ValueEnum};

#[path = "./common/criterion.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}

use clap::{Args, Parser, Subcommand, ValueEnum};

#[path = "./common/deno.rs"]
mod cli;

fn main() {
    let cli = cli::Opt::parse();
    std::hint::black_box(&cli);
}

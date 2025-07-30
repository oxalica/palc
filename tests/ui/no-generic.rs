#[derive(palc::Parser)]
pub struct Parser<T> {
    a: T,
}

#[derive(palc::Args)]
pub struct Args<T> {
    a: T,
}

#[derive(palc::Subcommand)]
pub enum Subcommand<T> {
    A(T),
}

fn main() {}

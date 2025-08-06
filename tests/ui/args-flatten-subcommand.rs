use palc::{Args, Subcommand};

#[derive(Args)]
struct Outer {
    #[command(flatten)]
    inner: Inner,
}

#[derive(Args)]
struct Inner {
    #[command(subcommand)]
    subcmd: Subcmd,
}

#[derive(Subcommand)]
enum Subcmd {}

fn main() {}

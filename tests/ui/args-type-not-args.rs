//! TODO: This triggers are many duplicated errors. Could be improved, but blocked on rustc.
//! WAIT: <https://github.com/rust-lang/rust/issues/144745>

#[derive(palc::Args)]
struct Cli1 {
    #[command(flatten)]
    deep: Deep,
}

struct Deep {}

#[derive(palc::Args)]
struct Cli2 {
    #[command(flatten)]
    deep: Subcmd,
}

#[derive(palc::Subcommand)]
enum Subcmd {}

fn main() {}

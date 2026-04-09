//! TODO: The error message mentions `__private` which is suboptimal.

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

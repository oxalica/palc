#[derive(palc::Args)]
struct Cli1 {
    #[command(subcommand)]
    deep: Deep,
}

struct Deep {}

#[derive(palc::Args)]
struct Cli2 {
    #[command(subcommand)]
    cmd: Subargs,
}

#[derive(palc::Args)]
struct Subargs {}

fn main() {}

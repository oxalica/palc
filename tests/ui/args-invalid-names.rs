#[derive(palc::Args)]
struct Cli {
    #[arg(long = "")]
    long_empty: i32,
    #[arg(long = "\0")]
    long_control: i32,
    #[arg(long = "-foo")]
    long_dash: i32,
    #[arg(long = "a=b")]
    long_eq: i32,

    #[arg(short = '-')]
    short_dash: i32,
    #[arg(short = '=')]
    short_eq: bool,
    #[arg(short = '\x1e')]
    short_control: bool,
}

fn main() {}

use palc::{Args, ValueEnum};

#[derive(Args)]
struct Cli1 {
    #[arg(long, value_enum, ignore_case = true)]
    lower: LowerCase,
    #[arg(long, value_enum, ignore_case = true)]
    upper: UpperCase,
}

#[derive(ValueEnum)]
enum LowerCase {
    Hello,
}

#[derive(ValueEnum)]
#[value(rename_all = "UPPER")]
enum UpperCase {
    Hello,
}

fn main() {}

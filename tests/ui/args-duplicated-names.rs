#[derive(palc::Args)]
struct Short {
    #[arg(short)]
    hello: i32,
    #[arg(short)]
    hell: i32,

    #[arg(long)]
    long1: i32,
    #[arg(long = "long1")]
    long2: i32,
}

fn main() {}

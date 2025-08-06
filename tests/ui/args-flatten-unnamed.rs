use palc::Args;

#[derive(Args)]
struct Outer {
    #[command(flatten)]
    inner: Inner,
}

#[derive(Args)]
struct Inner {
    unnamed: String,
}

fn main() {}

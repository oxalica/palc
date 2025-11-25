use palc::Args;

#[derive(Args)]
struct Outer {
    #[command(flatten)]
    inner: Inner,
}

#[derive(Args)]
struct Inner {
    arg: String,
}

fn main() {}

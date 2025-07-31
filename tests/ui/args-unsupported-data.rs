use palc::{__private::Args, Args};

#[derive(Args)]
struct Unit;

#[derive(Args)]
struct Tuple();

#[derive(Args)]
union Union {
    a: (),
}

#[derive(Args)]
enum Enum {}

struct AssertImplArgs
where
    Unit: Args,
    Tuple: Args,
    Enum: Args;

fn main() {}

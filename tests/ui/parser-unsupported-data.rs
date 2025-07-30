use palc::Parser;

#[derive(Parser)]
struct Unit;

#[derive(Parser)]
struct Tuple();

#[derive(Parser)]
union Union {
    a: (),
}

// TODO: top-level subcommands.
#[derive(Parser)]
enum Enum {}

struct AssertImplParser
where
    Unit: Parser,
    Tuple: Parser,
    Enum: Parser;

fn main() {}

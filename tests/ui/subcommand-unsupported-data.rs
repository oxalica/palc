use palc::Subcommand;

#[derive(Subcommand)]
struct Unit;

#[derive(Subcommand)]
struct Tuple();

#[derive(Subcommand)]
struct Named {
    a: (),
}

#[derive(Subcommand)]
union Union {
    a: (),
}

struct AssertImplSubcommand
where
    Unit: Subcommand,
    Tuple: Subcommand,
    Named: Subcommand,
    Enum: Subcommand;

fn main() {}

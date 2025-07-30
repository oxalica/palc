use palc::{__private::ValueEnum, ValueEnum};

#[derive(ValueEnum)]
struct Unit;

#[derive(ValueEnum)]
struct Tuple();

#[derive(ValueEnum)]
struct Named {}

#[derive(ValueEnum)]
union Union {
    a: (),
}

struct AssertImpl
where
    Unit: ValueEnum,
    Tuple: ValueEnum,
    Named: ValueEnum;

fn main() {}

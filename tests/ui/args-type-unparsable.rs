#[derive(palc::Args)]
struct Cli {
    positional: MyType,
    #[arg(long)]
    arg: MyType,

    vec: Vec<MyType>,
    #[arg(long)]
    vec_arg: Vec<MyType>,
}

#[derive(palc::Args)]
struct Cli2 {
    option_vec: Option<Vec<MyType>>,
    #[arg(long)]
    option_vec_arg: Option<Vec<MyType>>,
}

struct MyType;

fn main() {}

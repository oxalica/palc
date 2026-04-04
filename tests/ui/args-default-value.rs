use std::path::PathBuf;

#[derive(palc::Args)]
struct Cli {
    // Ok. A default string is provided.
    #[arg(default_value = "/")]
    default_string: PathBuf,

    // Ok. i32: Display.
    #[arg(default_value_t)]
    default_i32: i32,

    // Ok. ValueEnum does not require Display.
    #[arg(default_value_t)]
    value_enum: Enum,

    // TODO: Support this.
    // #[arg(default_value = "<filesystem root>", default_value_t = "/".into())]
    // both: PathBuf,

    // Error. PathBuf: !Display.
    #[arg(default_value_t = "/".into())]
    default_value: PathBuf,

    // Error. PathBuf: !Display.
    #[arg(default_value_t)]
    default_trait: PathBuf,

    // Error. Type mismatch.
    #[arg(default_value_t = ())]
    wrong_type: i32,
}

#[derive(palc::ValueEnum, Default)]
enum Enum {
    #[default]
    Foo,
}

fn main() {}

//! See `./common/simple.rs`.
use std::path::PathBuf;

use argh::FromArgs;

#[expect(dead_code, reason = "fields are only for testing")]
#[derive(FromArgs)]
/// My great app.
pub struct Cli {
    /// print more text
    #[argh(switch, short = 'v')]
    verbose: bool,
    /// the file to process
    #[argh(positional)]
    file: PathBuf,
}

fn main() {
    let cli: Cli = argh::from_env();
    std::hint::black_box(&cli);
}

//! This is an example for a dead simple program.
use std::path::PathBuf;

use super::Parser;

#[derive(Parser)]
/// My great app.
pub struct Cli {
    /// Print more text.
    #[arg(long, short)]
    verbose: bool,
    /// The file to process.
    file: PathBuf,
}

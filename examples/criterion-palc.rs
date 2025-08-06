use palc::{Parser, ValueEnum};

#[allow(dead_code)]
#[path = "./util/criterion.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}

#[cfg(feature = "help")]
#[test]
fn help() {
    let help = cli::Cli::render_long_help("me");
    println!("{help}");

    assert!(help.contains("Usage: me [OPTIONS] [FILTER]..."));
    assert!(help.contains("-c, --color <COLOR>"));
    assert!(help.contains("Configure coloring of output."));

    assert!(help.contains("This executable is a Criterion.rs benchmark."));
}

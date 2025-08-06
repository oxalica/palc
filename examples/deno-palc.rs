use palc::{Args, Parser, Subcommand, ValueEnum};

#[path = "./util/deno.rs"]
mod cli;

fn main() {
    let cli = cli::Opt::parse();
    std::hint::black_box(&cli);
}

#[cfg(feature = "help")]
#[test]
fn help() {
    let help = cli::Opt::render_long_help("me");
    println!("{help}");

    assert!(help.contains("A secure JavaScript and TypeScript runtime"));
    assert!(help.contains("Usage: me [OPTIONS] [COMMAND"));
    assert!(help.contains("-L, --log-level <LOG_LEVEL>"));

    // TODO: assert!(help.contains("ENVIRONMENT VARIABLES:"));
}

#[cfg(feature = "help")]
#[test]
fn help_subcommand() {
    let help = cli::Opt::try_parse_from(["me", "compile", "--help"]).err().unwrap().to_string();
    println!("{help}");

    assert!(help.contains("Compiles the given script into a self contained executable."));
    assert!(help.contains("Usage: me compile [OPTIONS]"));
    // assert!(help.contains("-L, --log-level <LOG_LEVEL>"));
}

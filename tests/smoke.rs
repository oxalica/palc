use std::path::PathBuf;

use palc::Parser as _;
use palc_derive::{Args, Parser, Subcommand, ValueEnum};

/// My great App.
#[derive(Debug, PartialEq, Parser)]
struct MyCli {
    name: Option<String>,

    #[command(flatten)]
    config: Config,

    #[arg(short = 'v', global = true)]
    debug: bool,

    #[arg(long, require_equals = true)]
    color: Color,

    #[command(subcommand)]
    command: Option<Commands>,
}

/// Configure something.
#[derive(Debug, PartialEq, Args)]
struct Config {
    #[arg(long)]
    config_file: Option<PathBuf>,
    #[arg(long)]
    config: Option<String>,
}

#[derive(Debug, PartialEq, Subcommand)]
enum Commands {
    /// Do something complex
    Test {
        #[arg(short, long)]
        list: bool,

        files: Option<Vec<PathBuf>>,
    },
    Transparent(Config),
    /// Do something simple
    Unit,
}

#[derive(Debug, PartialEq, ValueEnum)]
enum Color {
    Auto,
    Never,
    Always,
}

#[test]
fn smoke() {
    let args = MyCli::try_parse_from([
        "foo",
        "--color=always",
        "--config",
        "foo",
        "bar",
        "test",
        "-l",
        "-v",
        "hello",
        "world",
    ])
    .unwrap();
    assert_eq!(
        args,
        MyCli {
            color: Color::Always,
            name: Some("bar".into()),
            config: Config { config_file: None, config: Some("foo".into()) },
            debug: true,
            command: Some(Commands::Test {
                list: true,
                files: Some(vec![PathBuf::from("hello"), PathBuf::from("world")]),
            }),
        }
    );
}

#[cfg(feature = "help")]
#[test]
fn help() {
    let help = MyCli::render_long_help("me");
    println!("{help}");

    assert!(help.contains("My great App."));
    assert!(help.contains("Usage: me --color=<COLOR> [OPTIONS]"));
}

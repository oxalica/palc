#![cfg(feature = "help")]
#![expect(dead_code, reason = "only for help generation")]
use std::path::PathBuf;

use expect_test::{Expect, expect};
use palc::{Args, Parser, Subcommand, ValueEnum};

/// My great app.
#[derive(Parser)]
struct ArgsCli {
    /// Log more details.
    #[arg(short = 'v')]
    verbose: bool,

    #[arg(long, default_value_t)]
    color: Color,

    #[command(flatten)]
    flatten: Flatten,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Args)]
struct Flatten {
    /// The main config file path.
    #[arg(long)]
    config_file: PathBuf,
}

#[derive(Default, ValueEnum)]
enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

impl std::fmt::Display for Color {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unreachable!("should prefer `ValueEnum` instead of this")
    }
}

#[derive(Subcommand)]
enum Commands {
    /// Run the app.
    Run {
        /// Passthru arguments.
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Run some tests.
    Test {
        /// Filter patterns.
        filters: Vec<String>,
        #[arg(long)]
        feature: Vec<String>,
    },
    Config(ConfigCli),
}

/// Configure something.
///
/// Some detailed explanation.
#[derive(Args)]
#[command(after_long_help = "this command provides absolutely no warranty!")]
struct ConfigCli {
    #[arg(short, long, value_name = "VALUE")]
    key: String,

    /// Enable debug.
    #[arg(long)]
    debug: bool,

    /// Force to do it.
    #[arg(short)]
    force: bool,
}

#[track_caller]
fn assert_help<P: Parser>(args: &[&str], expect: Expect) {
    let help = P::try_parse_from(args).err().unwrap().try_into_help().unwrap();
    expect.assert_eq(&help);
}

#[test]
#[ignore = "FIXME"]
fn top_level() {
    assert_help::<ArgsCli>(
        &["me", "--help"],
        expect![[r#"
            My great app.

            Usage: me --config-file <CONFIG_FILE> [OPTIONS] <COMMAND>

            Commands:
                run     Run the app.
                test    Run some tests.
                config  Configure something.

            Options:
              -v
                      Log more details.

                  --color <COLOR>
                      [default: auto]

                  --config-file <CONFIG_FILE>
                      The main config file path.

        "#]],
    );
}

#[test]
fn subcommand() {
    assert_help::<ArgsCli>(
        &["me", "config", "--help"],
        expect![[r#"
            Configure something.
            Some detailed explanation.

            Usage: me config --key <VALUE> [OPTIONS]

            Options:
              -k, --key <VALUE>

                  --debug
                      Enable debug.

              -f
                      Force to do it.

            this command provides absolutely no warranty!"#]],
    );
}

#[test]
fn last() {
    assert_help::<ArgsCli>(
        &["me", "run", "--help"],
        expect![[r#"
            Run the app.

            Usage: me run [-- [ARGS]...]

            Arguments:
              [ARGS]...
                      Passthru arguments.

        "#]],
    );
}

#[test]
fn multiple() {
    assert_help::<ArgsCli>(
        &["me", "test", "--help"],
        expect![[r#"
            Run some tests.

            Usage: me test [OPTIONS] [FILTERS]...

            Arguments:
              [FILTERS]...
                      Filter patterns.

            Options:
                  --feature <FEATURE>

        "#]],
    );
}

#[test]
fn default_value() {
    #[derive(Parser)]
    struct Cli {
        #[arg(long, default_value = "invalid")]
        default_str: i32,
        #[arg(long, default_value_t)]
        default_trait: i32,
        /// Preferred!
        #[arg(long, default_value_t = 42)]
        default_expr: i32,
        #[arg(long, default_value_t = "foo".into())]
        infer: String,
        #[arg(default_value = "static")]
        positional: Option<String>,
    }

    assert_help::<Cli>(
        &["me", "--help"],
        expect![[r#"
            Usage: me [OPTIONS]

            Arguments:
              [POSITIONAL]
                      [default: static]

            Options:
                  --default-str <DEFAULT_STR>
                      [default: invalid]

                  --default-trait <DEFAULT_TRAIT>
                      [default: 0]

                  --default-expr <DEFAULT_EXPR>
                      Preferred!
                      [default: 42]

                  --infer <INFER>
                      [default: foo]

        "#]],
    );
}

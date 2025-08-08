#![cfg(feature = "help")]
#![expect(dead_code, reason = "only for help generation")]
use expect_test::{Expect, expect};
use palc::{Args, Parser, Subcommand};

/// My great app.
#[derive(Parser)]
struct ArgsCli {
    /// Log more details.
    #[arg(short = 'v')]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
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
}

#[track_caller]
fn assert_help<P: Parser>(args: &[&str], expect: Expect) {
    let help = P::try_parse_from(args).err().unwrap().try_into_help().unwrap();
    expect.assert_eq(&help);
}

#[test]
fn top_level() {
    assert_help::<ArgsCli>(
        &["me", "--help"],
        expect![[r#"
            My great app.

            Usage: me [OPTIONS] <COMMAND>

            Commands:
                run     Run the app.
                test    Run some tests.
                config  Configure something.

            Options:
              -v
                      Log more details.
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

            this command provides absolutely no warranty!"#]],
    );
}

#[test]
fn last() {
    assert_help::<ArgsCli>(
        &["me", "run", "--help"],
        expect![[r#"
            Run the app.

            Usage: me run -- [ARGS]...

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

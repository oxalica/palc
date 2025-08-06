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
    Run,
    /// Run some tests.
    Test {
        /// Filter patterns.
        filters: Vec<String>,
    },
    Config(ConfigCli),
}

/// Configure something.
///
/// Some detailed explanation.
#[derive(Args)]
struct ConfigCli {
    #[arg(short, long, value_name = "VALUE")]
    key: String,
}

#[track_caller]
fn assert_help<P: Parser>(args: &[&str], expect: Expect) {
    let help = P::try_parse_from(args).err().unwrap().try_into_help().unwrap();
    expect.assert_eq(&help);
}

#[test]
fn args() {
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
    assert_help::<ArgsCli>(
        &["me", "run", "--help"],
        expect![[r#"
            Usage: me run
        "#]],
    );
    assert_help::<ArgsCli>(
        &["me", "test", "--help"],
        expect![[r#"
            Usage: me test [FILTERS]...

            Arguments:
              [FILTERS]...          Filter patterns.
        "#]],
    );
    assert_help::<ArgsCli>(
        &["me", "config", "--help"],
        expect![[r#"
            Configure something.
            Some detailed explanation.

            Usage: me config --key <VALUE>

            Options:
              -k, --key <VALUE>

        "#]],
    );
}

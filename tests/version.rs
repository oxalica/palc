#![cfg(feature = "version")]
#![expect(dead_code, reason = "only for version generation")]
use expect_test::{Expect, expect};
use palc::{Parser, Subcommand};

/// My great app.
#[derive(Parser)]
#[command(version = "1.2.3")]
struct VersionCli {
    /// Log more details.
    #[arg(short = 'v')]
    verbose: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the app.
    Run,
}

#[track_caller]
fn assert_version<P: Parser>(args: &[&str], expect: Expect) {
    let version = P::try_parse_from(args).err().unwrap().try_into_version().unwrap();
    expect.assert_eq(&version);
}

#[test]
fn version_long() {
    assert_version::<VersionCli>(
        &["me", "--version"],
        expect![["me 1.2.3"]],
    );
}

#[test]
fn version_short() {
    assert_version::<VersionCli>(
        &["me", "-V"],
        expect![["me 1.2.3"]],
    );
}

#[test]
fn version_with_subcommand() {
    // --version should work even after a subcommand name.
    assert_version::<VersionCli>(
        &["me", "run", "--version"],
        expect![["me 1.2.3"]],
    );
}

#[test]
fn version_inherit() {
    /// Test app.
    #[derive(Parser)]
    #[command(version)]
    struct InheritVersionCli {}

    let version = InheritVersionCli::try_parse_from(["me", "--version"])
        .err()
        .unwrap()
        .try_into_version()
        .unwrap();
    // The inherited version is env!("CARGO_PKG_VERSION") from the test crate's
    // Cargo.toml, which resolves to palc's version "0.0.2".
    let expected = format!("me {}", env!("CARGO_PKG_VERSION"));
    assert_eq!(version, expected);
}

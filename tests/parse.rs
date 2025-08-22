use expect_test::{Expect, expect};
use palc::{Args, Parser, Subcommand};
use std::{ffi::OsString, fmt::Debug};

#[derive(Debug, Parser)]
struct CliEmpty {}

#[track_caller]
fn check<P: palc::Parser + Debug + PartialEq>(
    args: impl IntoIterator<Item = impl Into<OsString> + Clone>,
    expect: &P,
) {
    let got = P::try_parse_from(args).unwrap();
    assert_eq!(got, *expect);
}

#[track_caller]
fn check_err<P: palc::Parser + Debug>(
    args: impl IntoIterator<Item = impl Into<OsString> + Clone>,
    expect: Expect,
) {
    let ret = P::try_parse_from(args).unwrap_err();
    expect.assert_eq(&ret.to_string());
}

#[derive(Debug, Clone, PartialEq, Subcommand)]
enum Sub {
    Sub,
}

#[test]
fn argv0() {
    check_err::<CliEmpty>(None::<&str>, expect!["missing executable argument (argv[0])"]);
}

#[test]
fn short() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short)]
        verbose: bool,
        #[arg(short)]
        debug: bool,
        #[arg(short)]
        file: Option<String>,
    }

    check(["", "-dvf-"], &Cli { verbose: true, debug: true, file: Some("-".into()) });
    check(["", "-f=-", "-v"], &Cli { verbose: true, debug: false, file: Some("-".into()) });
    check(["", "-d", "-f", "-", "-v"], &Cli { verbose: true, debug: true, file: Some("-".into()) });
}

#[test]
fn require_equals() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(long, short, require_equals = true)]
        file: Option<String>,
        #[arg(short)]
        verbose: bool,
    }

    check_err::<Cli>(
        ["", "--file", "-"],
        expect!["equal sign is needed when assigning values for '-f, --file=<FILE>'"],
    );
    check_err::<Cli>(
        ["", "-f", "-"],
        expect!["equal sign is needed when assigning values for '-f, --file=<FILE>'"],
    );
    check_err::<Cli>(
        ["", "-vf", "-"],
        expect!["equal sign is needed when assigning values for '-f, --file=<FILE>'"],
    );
    check_err::<Cli>(
        ["", "-fv"],
        expect!["equal sign is needed when assigning values for '-f, --file=<FILE>'"],
    );

    check(["", "-f="], &Cli { verbose: false, file: Some("".into()) });
    check(["", "-f=v"], &Cli { verbose: false, file: Some("v".into()) });
    check(["", "-vf=v"], &Cli { verbose: true, file: Some("v".into()) });
    check(["", "--file="], &Cli { verbose: false, file: Some("".into()) });
    check(["", "--file=v"], &Cli { verbose: false, file: Some("v".into()) });
}

#[test]
fn required() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        // TODO: Reject bool as unnamed arguments. It is almost always an typo.
        #[arg(long)]
        key: String,
        file: String,
        #[command(subcommand)]
        sub: Sub,
    }

    check_err::<Cli>(
        ["", "path"],
        expect!["the argument '--key <KEY>' is required but not provided"],
    );
    check_err::<Cli>(
        ["", "--key", "value"],
        expect!["the argument '<FILE>' is required but not provided"],
    );
    check_err::<Cli>(
        ["", "--key", "value", "path"],
        expect!["the subcommand is required but not provided"],
    );
    check_err::<Cli>(
        ["", "path", "sub"],
        expect!["the argument '--key <KEY>' is required but not provided"],
    );

    check_err::<Cli>(
        ["", "--key", "value", "sub"],
        expect!["the argument '<FILE>' is required but not provided"],
    );

    check_err::<Cli>(
        ["", "--key", "value", "--key=value"],
        expect!["the argument '--key <KEY>' cannot be used multiple times"],
    );

    let expect = Cli { key: "value".into(), file: "path".into(), sub: Sub::Sub };
    check(["", "path", "--key", "value", "sub"], &expect);
    check(["", "--key", "value", "path", "sub"], &expect);
}

#[test]
fn optional() {
    #[derive(Debug, Clone, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        flag: bool,
        #[arg(long)]
        key: Option<String>,
        file: Option<String>,
        #[command(subcommand)]
        sub: Option<Sub>,
    }

    let default = Cli::default();
    check([""], &default);
    check(["", "--flag"], &Cli { flag: true, ..default.clone() });
    check(["", "--key", "value"], &Cli { key: Some("value".into()), ..default.clone() });
    check(["", "path"], &Cli { file: Some("path".into()), ..default.clone() });

    check(["", "sub"], &Cli { sub: Some(Sub::Sub), ..default.clone() });

    check(
        ["", "--key", "sub", "path"],
        &Cli { key: Some("sub".into()), file: Some("path".into()), ..default.clone() },
    );
    check(
        ["", "path", "sub"],
        &Cli { file: Some("path".into()), sub: Some(Sub::Sub), ..default.clone() },
    );

    check(
        ["", "--flag", "--key", "value", "path", "sub"],
        &Cli {
            flag: true,
            key: Some("value".into()),
            file: Some("path".into()),
            sub: Some(Sub::Sub),
        },
    );

    check_err::<Cli>(
        ["", "--key", "value", "--key=value"],
        expect!["the argument '--key <KEY>' cannot be used multiple times"],
    );
    check_err::<Cli>(
        ["", "--flag", "--flag"],
        expect!["the argument '--flag' cannot be used multiple times"],
    );
}

#[test]
fn option_option() {
    #[derive(Debug, Clone, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        foo: Option<String>,
        #[arg(long)]
        bar: Option<Option<String>>,
    }

    check([""], &Cli { foo: None, bar: None });
    check(["", "--foo=", "--bar="], &Cli { foo: Some("".into()), bar: Some(None) });
    check(["", "--foo=a", "--bar=b"], &Cli { foo: Some("a".into()), bar: Some(Some("b".into())) });

    // They both expect a value, even if an empty one. Or it will fail.
    // This behavior matches clap.
    check_err::<Cli>(
        ["", "--foo"],
        expect!["a value is required for '--foo <FOO>' but none was supplied"],
    );
    check_err::<Cli>(
        ["", "--bar"],
        expect!["a value is required for '--bar <BAR>' but none was supplied"],
    );
}

#[test]
fn default_values() {
    #[derive(Debug, Clone, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(short = 'O', default_value_t)]
        opt: i32,
        #[arg(long, default_value = "none", conflicts_with = "opt")]
        debug: String,
    }

    // Constraints are validated before default values.
    check([""], &Cli { opt: 0, debug: "none".into() });
    check(["", "-O2"], &Cli { opt: 2, debug: "none".into() });
    check(["", "--debug=full"], &Cli { opt: 0, debug: "full".into() });

    check_err::<Cli>(
        ["", "-O2", "--debug="],
        expect!["the argument '--debug <DEBUG>' cannot be used with some other arguments"],
    );
}

#[test]
fn counter() {
    #[derive(Debug, Clone, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(long, short)]
        verbose: u8,
    }

    check([""], &Cli { verbose: 0 });
    check(["", "-vv"], &Cli { verbose: 2 });
    check(["", "--verbose", "-vv", "--verbose"], &Cli { verbose: 4 });
}

#[test]
fn flatten() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        verbose: bool,
        #[command(flatten)]
        config: Config,
        #[arg(long)]
        debug: bool,
    }

    #[derive(Debug, PartialEq, Args)]
    struct Config {
        #[arg(long)]
        config: Option<String>,
        #[arg(long)]
        config_file: Option<String>,
        #[arg(short, required = true)]
        force: bool,
    }

    check(
        ["", "--debug", "--verbose", "--config", "a=b", "-f"],
        &Cli {
            debug: true,
            verbose: true,
            config: Config { config: Some("a=b".into()), config_file: None, force: true },
        },
    );
    check(
        ["", "--config-file", "path", "--debug", "--verbose", "-f"],
        &Cli {
            debug: true,
            verbose: true,
            config: Config { config_file: Some("path".into()), config: None, force: true },
        },
    );

    // Check for arg index offsets.

    // Value errors.
    check_err::<Cli>(
        ["", "--config"],
        expect!["a value is required for '--config <CONFIG>' but none was supplied"],
    );
    // Validation errors.
    check_err::<Cli>(
        ["", "--config", "foo"],
        expect!["the argument '-f' is required but not provided"],
    );
}

#[test]
fn unknown_args() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short, long)]
        verbose: bool,
        file: String,
    }

    #[derive(Debug, PartialEq, Parser)]
    struct CliWithSub {
        #[arg(long)]
        verbose: bool,
        file: String,
        #[command(subcommand)]
        sub: Sub,
    }

    check_err::<Cli>(["", "--debug"], expect!["unexpected argument '--debug'"]);
    check_err::<Cli>(["", "-d"], expect!["unexpected argument '-d'"]);
    check_err::<Cli>(["", "-vd"], expect!["unexpected argument '-d'"]);
    check_err::<Cli>(["", "-v坏"], expect!["unexpected argument '-坏'"]);

    check_err::<Cli>(["", "path1", "path2"], expect!["unexpected argument 'path2'"]);

    check_err::<CliWithSub>(["", "path1", "path2"], expect!["unrecognized subcommand 'path2'"]);
    check_err::<CliWithSub>(["", "sub", "path"], expect!["unexpected argument 'path'"]);
}

#[cfg(unix)]
#[test]
fn non_utf8() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::{OsStrExt, OsStringExt};
    use std::path::PathBuf;

    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(short, long)]
        config: Option<PathBuf>,
        file: Option<PathBuf>,
        #[arg(short)]
        verbose: bool,
    }

    fn concat_os(a: impl AsRef<OsStr>, b: impl AsRef<OsStr>) -> OsString {
        let mut buf = a.as_ref().as_bytes().to_vec();
        buf.extend_from_slice(b.as_ref().as_bytes());
        OsString::from_vec(buf)
    }

    let non_utf8 = || OsString::from_vec(vec![0xFF]);
    assert!(non_utf8().to_str().is_none());
    check(
        ["".into(), non_utf8()],
        &Cli { file: Some(non_utf8().into()), config: None, verbose: false },
    );

    let mut exp = Cli { config: Some(non_utf8().into()), file: None, verbose: false };
    check([OsString::new(), "--config".into(), non_utf8()], &exp);
    check([OsString::new(), concat_os("--config=", non_utf8())], &exp);
    check([OsString::new(), concat_os("-c", non_utf8())], &exp);
    check([OsString::new(), concat_os("-c=", non_utf8())], &exp);

    exp.verbose = true;
    check([OsString::new(), concat_os("-vc", non_utf8())], &exp);
    check([OsString::new(), concat_os("-vc=", non_utf8())], &exp);
}

#[test]
fn global() {
    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        // Not global.
        #[arg(short)]
        verbose: bool,
        #[arg(short, long, global = true)]
        debug: Option<u8>,
        #[command(subcommand)]
        sub: Sub2,
    }

    #[derive(Debug, PartialEq, Subcommand)]
    enum Sub2 {
        Empty,
        Deep {
            #[arg(short)]
            verbose: bool,
        },
    }

    check(["", "empty"], &Cli { verbose: false, debug: None, sub: Sub2::Empty });
    check(["", "-v", "empty"], &Cli { verbose: true, debug: None, sub: Sub2::Empty });

    // Not global.
    check_err::<Cli>(["", "empty", "-v"], expect!["unexpected argument '-v'"]);

    // TODO: Is this behavior expected?
    check(
        ["", "-v", "deep"],
        &Cli { verbose: true, debug: None, sub: Sub2::Deep { verbose: false } },
    );
    check(
        ["", "deep", "-v"],
        &Cli { verbose: false, debug: None, sub: Sub2::Deep { verbose: true } },
    );
    check(
        ["", "-v", "deep", "-v"],
        &Cli { verbose: true, debug: None, sub: Sub2::Deep { verbose: true } },
    );

    check(["", "-d2", "empty"], &Cli { verbose: false, debug: Some(2), sub: Sub2::Empty });
    check(["", "-d", "2", "empty"], &Cli { verbose: false, debug: Some(2), sub: Sub2::Empty });
    check(
        ["", "-d2", "deep"],
        &Cli { verbose: false, debug: Some(2), sub: Sub2::Deep { verbose: false } },
    );
    check(
        ["", "-d", "2", "deep"],
        &Cli { verbose: false, debug: Some(2), sub: Sub2::Deep { verbose: false } },
    );

    check_err::<Cli>(
        ["", "-d2", "deep", "-d0"],
        expect!["the argument '-d, --debug <DEBUG>' cannot be used multiple times"],
    );
}

#[test]
fn hyphen_named() {
    #[derive(Debug, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(long)]
        no: Option<String>,
        #[arg(long, allow_hyphen_values = true)]
        yes: Option<String>,
        #[arg(long, allow_negative_numbers = true)]
        number: Option<i32>,

        #[arg(short = '1')]
        one: bool,
        #[arg(short)]
        flag: bool,
    }

    check_err::<Cli>(
        ["", "--no", "-1"],
        expect!["a value is required for '--no <NO>' but none was supplied"],
    );
    check_err::<Cli>(
        ["", "--no", "-f"],
        expect!["a value is required for '--no <NO>' but none was supplied"],
    );

    check_err::<Cli>(
        ["", "--number", "-f"],
        expect!["a value is required for '--number <NUMBER>' but none was supplied"],
    );
    check(["", "--number", "-1"], &Cli { number: Some(-1), ..Cli::default() });

    check(["", "--yes", "-1"], &Cli { yes: Some("-1".into()), ..Cli::default() });
    check(["", "--yes", "-f"], &Cli { yes: Some("-f".into()), ..Cli::default() });
}

#[test]
fn trailing_args() {
    #[derive(Debug, Default, PartialEq, Parser)]
    struct No {
        #[arg(short, default_value_t)]
        debug: i8,
        #[arg(trailing_var_arg = true)]
        any: Vec<String>,
    }

    #[derive(Debug, Default, PartialEq, Parser)]
    struct Yes {
        #[arg(short, default_value_t, allow_hyphen_values = true)]
        debug: i8,
        #[arg(trailing_var_arg = true)]
        // TODO: allow_hyphen_values
        any: Vec<String>,
    }

    // Implicit allow_hyphen_values when already entered a trailing_var_args.
    check(
        ["", "-d", "1", "a", "-d", "-1"],
        &No { debug: 1, any: vec!["a".into(), "-d".into(), "-1".into()] },
    );
    check_err::<No>(["", "-d", "-1"], expect!["a value is required for '-d <DEBUG>' but none was supplied"]);
    check_err::<No>(["", "-x"], expect!["unexpected argument '-x'"]);

    check(["", "-d", "-1"], &Yes { any: vec![], debug: -1 });
    // TODO: check(["", "-x", "-d", "-1"], &Yes { any: vec!["-x".into(), "-d".into(), "-1".into()], debug: 0 });
}

#[test]
fn value_delimiter() {
    #[derive(Debug, Default, PartialEq, Parser)]
    struct Cli {
        #[arg(short = 'F', long, use_value_delimiter = true)]
        features: Vec<String>,
    }

    check(
        ["", "--features", "a,b", "-F", "c", "-F=d,e", "--features="],
        &Cli { features: ["a", "b", "c", "d", "e", ""].map(Into::into).into() },
    );
}

#[test]
fn constraint() {
    #[derive(Debug, Default, PartialEq, Parser)]
    struct Required {
        #[arg(long, required = true)]
        key: Vec<String>,
        #[arg(required = true)]
        files: Vec<String>,
        #[arg(short, required = true)]
        force: bool,
        #[arg(short, required = true)]
        verbose: u8,
    }

    check_err::<Required>([""], expect!["the argument '--key <KEY>' is required but not provided"]);
    check_err::<Required>(
        ["", "--key=foo"],
        expect!["the argument '-f' is required but not provided"],
    );
    check_err::<Required>(
        ["", "--key=foo", "path"],
        expect!["the argument '-f' is required but not provided"],
    );
    check_err::<Required>(
        ["", "--key=foo", "path", "-f"],
        expect!["the argument '-v' is required but not provided"],
    );

    check(
        ["", "--key=foo", "path", "-fvv"],
        &Required { key: vec!["foo".into()], files: vec!["path".into()], force: true, verbose: 2 },
    )
}

#[test]
#[deny(unreachable_code)]
fn empty_subcommand() {
    #[derive(Debug, Parser, PartialEq)]
    struct Cli {
        #[command(subcommand)]
        cmd: Option<Subcmd>,
    }

    #[derive(Debug, Subcommand, PartialEq)]
    enum Subcmd {}

    check([""], &Cli { cmd: None });
    check_err::<Cli>(["", ""], expect!["unrecognized subcommand ''"]);
}

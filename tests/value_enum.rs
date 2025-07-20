use palc::__private::ValueEnum;
use palc::{Parser, ValueEnum};

#[derive(Debug, ValueEnum)]
enum Empty {}

#[derive(Debug, PartialEq, ValueEnum)]
enum Single {
    SingleVariant,
}

#[derive(Debug, PartialEq, ValueEnum)]
#[value(rename_all = "camelCase")]
enum Override {
    #[value(name = "")]
    A,
    DefaultVariant,
    #[value(name = "the value")]
    ExplicitVariant,
}

#[test]
fn empty() {
    assert!(Empty::parse_value("").is_none())
}

#[test]
fn single() {
    assert_eq!(Single::parse_value("single-variant"), Some(Single::SingleVariant));
    assert_eq!(Single::parse_value("Single-variant"), None);
}

#[test]
fn rename_all() {
    assert_eq!(Override::parse_value(""), Some(Override::A));
    assert_eq!(Override::parse_value("defaultVariant"), Some(Override::DefaultVariant));
    assert_eq!(Override::parse_value("the value"), Some(Override::ExplicitVariant));
}

macro_rules! test_renames {
    ($($convert:literal, $variant:ident, $expect:literal;)*) => {
        $(
            #[expect(non_snake_case)]
            #[test]
            fn $variant() {
                #[allow(non_camel_case_types)]
                #[derive(Debug, PartialEq, ValueEnum)]
                #[value(rename_all = $convert)]
                enum $variant {
                    $variant,
                }

                assert_eq!($variant::parse_value($expect), Some($variant::$variant));
            }
        )*
    };
}

test_renames! {
    "camelCase", RenameCamel, "renameCamel";
    "PascalCase", RenamePascal, "RenamePascal";
    "SCREAMING_SNAKE_CASE", RenameScream, "RENAME_SCREAM";
    "snake_case", RenameSnake, "rename_snake";
    "lower", RenameLower, "renamelower";
    "UPPER", RenameUpper, "RENAMEUPPER";
    "verbatim", RenameXMLHttp_Request, "RenameXMLHttp_Request";
}

#[test]
fn ignore_case() {
    #[derive(Debug, PartialEq, ValueEnum)]
    enum Enum {
        HelloWorld,
    }

    #[derive(Debug, PartialEq, Parser)]
    struct Cli {
        #[arg(long, ignore_case = true)]
        key: Enum,
    }

    let got = Cli::try_parse_from(["", "--key", "hElLo-WoRlD"]).unwrap();
    assert_eq!(got, Cli { key: Enum::HelloWorld });
}

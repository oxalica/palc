use crate::{refl::RawArgsInfo, runtime::ParserChainNode};

#[inline(never)]
fn push_str(out: &mut String, s: &str) {
    out.push_str(s);
}

fn collect_subcmds(out: &mut Vec<(String, &'static RawArgsInfo)>, chain: ParserChainNode) {
    let info = chain.state.info();
    let cmd_name = chain.cmd_name.to_string_lossy().into_owned();
    out.push((cmd_name, info));
    if let Some(deep) = chain.ancestors.out() {
        collect_subcmds(out, deep);
    }
}

#[cold]
pub(crate) fn render_help_into(out: &mut String, chain: &mut ParserChainNode) {
    macro_rules! w {
        ($($e:expr),*) => {{
            $(push_str(out, $e);)*
        }};
    }

    let path = {
        let mut path = Vec::with_capacity(8);
        collect_subcmds(
            &mut path,
            ParserChainNode {
                cmd_name: chain.cmd_name,
                state: chain.state,
                ancestors: chain.ancestors,
            },
        );
        path.reverse();
        path
    };

    // There must be at least a top-level `Parser` info, or we would fail fast by `MissingArg0`.
    assert!(!path.is_empty());
    let info = path.last().unwrap().1;

    // About this (sub)command.
    let doc = info.doc();
    if !doc.long_about.is_empty() {
        w!(doc.long_about, "\n\n");
    }

    // Usage of current subcommand path.

    w!("Usage:");
    // Argv0 is included.
    for (cmd, _) in path.iter() {
        w!(" ", cmd);
    }

    let mut has_named @ mut has_opt_named @ mut has_unnamed = false;
    // TODO: Global args.
    for arg in info.named_args() {
        has_named = true;
        if arg.required() {
            let mut desc = arg.description();
            if matches!(desc.as_bytes(), [b'-', short, _, _, ..] if *short != b'-') {
                // `-s, --long <VAL>` => `--long <VAL>`
                desc = &desc[4..];
            }
            w!(" ", desc);
        } else {
            has_opt_named = true;
        }
    }
    if has_opt_named {
        w!(" [OPTIONS]");
    }
    for arg in info.unnamed_args() {
        has_unnamed = true;
        w!(" ", arg.description());
    }
    let subcmds = info.subcommands();
    if subcmds.is_some() {
        w!(if info.subcommand_optional() { " [COMMAND]" } else { " <COMMAND>" });
    }
    w!("\n");

    // List of commands.

    if let Some(subcmds) = subcmds {
        w!("\nCommands:\n");
        let pad = "                        ";
        let max_len = subcmds.clone().map(|(cmd, _)| cmd.len()).max().unwrap_or(0);

        // Note: Only short help is displayed for the subcommand list.
        for (cmd, long_about) in subcmds {
            w!("    ", cmd);
            if !long_about.is_empty() {
                let short_about = long_about.split_terminator('\n').next().unwrap_or(long_about);
                let pad_len = max_len.saturating_sub(cmd.len()) + 2;
                let pad = &pad[..pad.len().min(pad_len)];
                w!("", pad, short_about);
            }
            w!("\n");
        }
    }

    // List of unnamed arguments.

    if has_unnamed {
        w!("\nArguments:\n");
        let mut first = true;
        for arg in info.unnamed_args() {
            if first {
                first = false;
            } else {
                w!("\n");
            }
            w!("  ", arg.description());
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j > 0 {
                        w!("\n");
                    }
                    w!("          ", s, "\n");
                }
            }
        }
    }

    // List of named arguments.

    if has_named {
        w!("\nOptions:\n");

        for arg in info.named_args() {
            let padding = if arg.description().starts_with("--") { "      " } else { "  " };
            w!(padding, arg.description(), "\n");
            if let Some(help) = arg.long_help() {
                for (j, s) in help.split_terminator('\n').enumerate() {
                    if j != 0 {
                        w!("\n");
                    }
                    w!("          ", s, "\n");
                }
            }
            w!("\n");
        }
    }

    if !doc.after_long_help.is_empty() {
        w!(doc.after_long_help);
    }
}

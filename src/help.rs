use crate::{
    refl::{FMT_NAMED, FMT_UNNAMED, FMT_USAGE_NAMED, FMT_USAGE_UNNAMED, RawArgsInfo},
    runtime::ParserChainNode,
};

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

    let fmt = info.fmt_help();

    // TODO: Global args.
    fmt(out, FMT_USAGE_NAMED);
    if info.has_optional_named() {
        w!(" [OPTIONS]");
    }
    fmt(out, FMT_USAGE_UNNAMED);

    let subcmds = info.subcommands();
    if subcmds.is_some() {
        w!(if info.subcommand_optional() { " [COMMAND]" } else { " <COMMAND>" });
    }
    // EOL and empty line separator.
    w!("\n\n");

    // List of commands.

    if let Some(subcmds) = subcmds {
        w!("Commands:\n");
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

        // Empty line separator.
        w!("\n");
    }

    // List of unnamed arguments.
    {
        let last = out.len();
        w!("Arguments:\n");
        let banner = out.len();
        fmt(out, FMT_UNNAMED);
        if out.len() == banner {
            out.truncate(last);
        }
        // Empty line separator should already be emitted.
    }

    // List of named arguments.
    {
        let last = out.len();
        w!("Options:\n");
        let banner = out.len();
        fmt(out, FMT_NAMED);
        if out.len() == banner {
            out.truncate(last);
        }
        // Empty line separator should already be emitted.
    }

    w!(doc.after_long_help);
}

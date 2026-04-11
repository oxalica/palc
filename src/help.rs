use crate::{
    runtime::{ArgsFrame, Frame},
    util::split_once,
};

#[inline(never)]
fn push_str(out: &mut String, s: &str) {
    out.push_str(s);
}

macro_rules! writefmt {
    ($out:expr, $($tt:tt)*) => {
        _ = std::fmt::write($out, format_args!($($tt)*))
    };
}

#[cold]
pub(crate) fn render_help_into(out: &mut String, frame: &ArgsFrame) {
    macro_rules! w {
        ($($e:expr),*) => {{
            $(push_str(out, $e);)*
        }};
    }

    let info = frame.info;
    let help_fmt = info.help;

    // About this (sub)command.
    {
        let pos = out.len();
        writefmt!(out, "{help_fmt:.4}");
        if out.len() != pos {
            w!("\n\n");
        }
    }

    // Usage of current subcommand path.

    w!("Usage:");
    frame.collect_command_prefix(out);

    // TODO: Global args.
    writefmt!(
        out,
        "{help_fmt:.0}{}{help_fmt:.1}",
        if info.has_optional_named { " [OPTIONS]" } else { "" },
    );

    let subcmds = info.subcommands_with_help();
    if subcmds.is_some() {
        w!(if info.is_subcmd_optional { " [COMMAND]" } else { " <COMMAND>" });
    }
    // EOL and empty line separator.
    w!("\n\n");

    // List of commands.

    if let Some(subcmds) = subcmds {
        w!("Commands:\n");
        let pad = "                        ";
        let max_len = subcmds.clone().map(|(cmd, _)| cmd.len()).max().unwrap_or(0);

        // Note: Only short help is displayed for the subcommand list.
        for (cmd, subcmd_help_fmt) in subcmds {
            w!("    ", cmd);
            let pad_len = max_len.saturating_sub(cmd.len()) + 2;
            let pad = &pad[..pad.len().min(pad_len)];

            let pos = out.len();
            // 4: long_about
            writefmt!(out, "{pad}{subcmd_help_fmt:.4}");
            if out.len() == pos {
                out.truncate(pos - pad.len());
            } else if let Some((lhs, _)) = split_once(&out[pos..], b'\n') {
                // Only keep the first line as summary.
                out.truncate(pos + lhs.len());
            }
            w!("\n");
        }

        // Empty line separator.
        w!("\n");
    }

    // List of positional arguments.
    {
        let last = out.len();
        w!("Arguments:\n");
        let banner = out.len();
        // 3: Positional argument long help.
        writefmt!(out, "{help_fmt:.3}");
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
        // 2: Named argument long help.
        writefmt!(out, "{help_fmt:.2}");
        if out.len() == banner {
            out.truncate(last);
        }
        // Empty line separator should already be emitted.
    }

    writefmt!(out, "{help_fmt:.5}");
}

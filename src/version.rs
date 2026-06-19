use crate::runtime::{ArgsFrame, Frame};

#[cold]
pub(crate) fn render_version_into(out: &mut String, frame: &ArgsFrame) {
    if let Some(name) = frame.program_name() {
        out.push_str(name);
        out.push(' ');
    }
    // `version()` is known to be `Some` since the caller already checked
    // `frame.version()` before constructing `ErrorKind::Version`.
    if let Some(version) = frame.version() {
        out.push_str(version);
    }
}

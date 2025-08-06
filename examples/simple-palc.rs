use palc::Parser;

#[allow(dead_code)]
#[path = "./util/simple.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}

#[cfg(feature = "help")]
#[test]
fn help() {
    let help = cli::Cli::render_long_help("me");
    println!("{help}");
    assert!(help.contains("Usage: me [OPTIONS] <FILE>"));
}

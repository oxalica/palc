use palc::Parser;

#[expect(dead_code, reason = "fields are unused and just for testing")]
#[path = "./common/simple.rs"]
mod cli;

fn main() {
    let cli = cli::Cli::parse();
    std::hint::black_box(&cli);
}

#[cfg(feature = "full-featured")]
#[test]
fn help() {
    let help = cli::Cli::render_long_help("me");
    println!("{help}");
    assert!(help.contains("Usage: me [OPTIONS] <FILE>"));
}

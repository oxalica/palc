//! This is an example for tons of arguments but no groups or subcommands.
//!
//! Manually translated from:
//! <https://github.com/bheisler/criterion.rs/blob/260e2f1c788227096cf483c0c2a1933968bbe27b/src/lib.rs#L754>
//!
//! Criterion.rs is dual licensed under the Apache 2.0 license and the MIT license.
//! See more at <https://github.com/bheisler/criterion.rs>
use std::num::NonZero;

use super::{Parser, ValueEnum};

#[derive(Parser)]
#[command(
    name = "Criterion Benchmark",
    after_long_help = "
This executable is a Criterion.rs benchmark.
See https://github.com/bheisler/criterion.rs for more details.

To enable debug output, define the environment variable CRITERION_DEBUG.
Criterion.rs will output more debug information and will save the gnuplot
scripts alongside the generated plots.

To test that the benchmarks work, run `cargo test --benches`

NOTE: If you see an 'unrecognized option' error using any of the options above, see:
https://bheisler.github.io/criterion.rs/book/faq.html
"
)]
pub struct Cli {
    /// Skip benchmarks whose names do not contain FILTER.
    filter: Vec<String>,
    /// Configure coloring of output. always = always colorize output, never = never colorize
    /// output, auto = colorize output if output is a tty and compiled for unix.
    #[arg(short, long, alias = "colour", value_enum, default_value_t)]
    color: Color,
    /// Print additional statistical information.
    #[arg(short, long)]
    verbose: bool,
    /// Print only the benchmark results.
    #[arg(short, long, conflicts_with = "verbose")]
    quiet: bool,
    /// Disable plut and HTML generation.
    #[arg(short, long)]
    noplot: bool,
    /// Save results under a named baseline.
    #[arg(short, long, default_value = "base")]
    save_baseline: String,
    /// Discard benchmark results.
    #[arg(long, conflicts_with_all = ["save_baseline", "baseline", "baseline_lenient"])]
    discard_baseline: bool,
    /// Compare to a named baseline. If any benchmarks do not have the specified baseline this
    /// command fails.
    #[arg(short, long, conflicts_with_all = ["save_baseline", "baseline_lenient"])]
    baseline: Option<String>,
    #[arg(long, conflicts_with_all = ["save_baseline", "baseline"])]
    baseline_lenient: Option<String>,
    /// List all benchmarks
    #[arg(long, conflicts_with_all = ["test", "profile_time"])]
    list: bool,
    /// Output formatting
    #[arg(long, value_enum, default_value_t)]
    format: Format,
    /// List or run ignored benchmarks (currently means skip all benchmarks)
    #[arg(long)]
    ignored: bool,
    /// Run benchmarks that exactly match the provided filter
    #[arg(long)]
    exact: bool,
    /// Iterate each benchmark for approximately the given number of seconds, doing no analysis and
    /// without storing the results. Useful for running the benchmarks in a profiler.
    #[arg(long, conflicts_with_all = ["test", "list"])]
    profile_time: Option<f64>,
    /// Load a previous baseline instead of sampling new data.
    #[arg(long, conflicts_with = "profile_time", requires = "baseline")]
    load_baseline: Option<String>,
    /// Changes the default size of the sample for this run.
    #[arg(long, default_value_t = 100)]
    sample_size: usize,
    /// Changes the default warm up time for this run.
    #[arg(long, default_value_t = 3.0)]
    warm_up_time: f64,
    /// Changes the default measurement time for this run.
    #[arg(long, default_value_t = 5.0)]
    measurement_time: f64,
    /// Changes the default number of resamples for this run.
    #[arg(long, default_value_t = 100000.try_into().unwrap())]
    nresamples: NonZero<usize>,
    /// Changes the default noise threshold for this run.
    #[arg(long, default_value_t = 0.01)]
    noise_threshld: f64,
    /// Changes the default confidence level for this run.
    #[arg(long, default_value_t = 0.95)]
    confidence_level: f64,
    /// Changes the default significance level for this run.
    #[arg(long, default_value_t = 0.05)]
    significance_level: f64,
    /// Benchmark only until the significance level has been reached
    #[arg(long, conflicts_with = "sample_size")]
    quick: bool,
    /// Run the benchmarks once, to verify that they execute successfully, but do not measure or
    /// report the results.
    #[arg(long, hide = true, conflicts_with_all = ["list", "profile_time"])]
    test: bool,
    #[arg(long, hide = true)]
    bench: Option<String>,
    /// Set the plotting backend. By default, Criterion.rs will use the gnuplot backend if gnuplot
    /// is available, or the plotters backend if it isn't.
    #[arg(long)]
    plotting_backend: Option<PlottingBackend>,
    /// Change the CLI output format. By default, Criterion.rs will use its own format. If output
    /// format is set to 'bencher', Criterion.rs will print output in a format that resembles the
    /// 'bencher' crate.
    #[arg(long, value_enum, default_value_t)]
    output_format: OutputFormat,
    /// Ignored, but added for compatibility with libtest.
    #[arg(long, hide = true)]
    nocapture: bool,
    /// Ignored, but added for compatibility with libtest.
    #[arg(long, hide = true)]
    show_output: bool,
    /// Ignored, but added for compatibility with libtest.
    #[arg(long, hide = true)]
    include_ignored: bool,
    #[arg(long, short = 'V', hide = true)]
    version: bool,
}

#[derive(Default, Clone, ValueEnum)]
enum Color {
    #[default]
    Auto,
    Always,
    Never,
}

#[derive(Default, Clone, ValueEnum)]
enum Format {
    #[default]
    Pretty,
    Terse,
}

#[derive(Clone, ValueEnum)]
enum PlottingBackend {
    Gnuplot,
    Plotters,
}

#[derive(Default, Clone, ValueEnum)]
enum OutputFormat {
    #[default]
    Criterion,
    Test,
}

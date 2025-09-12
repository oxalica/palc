//! See `./criterion-clap.rs`.
#![expect(dead_code, reason = "fields are only for testing")]
use std::str::FromStr;

use argh::FromArgs;

/// Criterion Benchmark
#[derive(FromArgs)]
struct Cli {
    /// skip benchmarks whose names do not contain FILTER.
    #[argh(positional)]
    filter: Vec<String>,
    /// configure coloring of output. always = always colorize output, never = never colorize
    /// output, auto = colorize output if output is a tty and compiled for unix.
    #[argh(option, short = 'c', default = "Color::Auto")]
    color: Color,
    /// print additional statistical information.
    #[argh(switch, short = 'v')]
    verbose: bool,
    /// print only the benchmark results.
    #[argh(switch, short = 'q')]
    quiet: bool,
    /// disable plut and HTML generation.
    #[argh(switch, short = 'n')]
    noplot: bool,
    /// save results under a named baseline.
    #[argh(option, short = 's')]
    save_baseline: Option<String>,
    /// discard benchmark results.
    #[argh(switch)]
    discard_baseline: bool,
    /// compare to a named baseline. If any benchmarks do not have the specified baseline this
    /// command fails.
    #[argh(option, short = 'b')]
    baseline: Option<String>,
    /// baseline lenient
    #[argh(option)]
    baseline_lenient: Option<String>,
    /// list all benchmarks
    #[argh(switch)]
    list: bool,
    /// output formatting
    #[argh(option, default = "Format::Pretty")]
    format: Format,
    /// list or run ignored benchmarks (currently means skip all benchmarks)
    #[argh(switch)]
    ignored: bool,
    /// run benchmarks that exactly match the provided filter
    #[argh(switch)]
    exact: bool,
    /// iterate each benchmark for approximately the given number of seconds, doing no analysis and
    /// without storing the results. Useful for running the benchmarks in a profiler.
    #[argh(option)]
    profile_time: Option<f64>,
    /// load a previous baseline instead of sampling new data.
    #[argh(option)]
    load_baseline: Option<String>,
    /// changes the default size of the sample for this run.
    #[argh(option)]
    sample_size: Option<usize>,
    /// changes the default warm up time for this run.
    #[argh(option, default = "3.0")]
    warm_up_time: f64,
    /// changes the default measurement time for this run.
    #[argh(option, default = "5.0")]
    measurement_time: f64,
    /// changes the default number of resamples for this run.
    #[argh(option, default = "100000")]
    nresamples: usize,
    /// changes the default noise threshold for this run.
    #[argh(option, default = "0.01")]
    noise_threshld: f64,
    /// changes the default confidence level for this run.
    #[argh(option, default = "0.95")]
    confidence_level: f64,
    /// changes the default significance level for this run.
    #[argh(option, default = "0.05")]
    significance_level: f64,
    /// benchmark only until the significance level has been reached
    #[argh(switch)]
    quick: bool,
    /// run the benchmarks once, to verify that they execute successfully, but do not measure or
    /// report the results.
    #[argh(switch, hidden_help)]
    test: bool,
    #[argh(option, hidden_help)]
    bench: Option<String>,
    /// set the plotting backend. By default, Criterion.rs will use the gnuplot backend if gnuplot
    /// is available, or the plotters backend if it isn't.
    #[argh(option, hidden_help)]
    plotting_backend: Option<String>,
    /// change the CLI output format. By default, Criterion.rs will use its own format. If output
    /// format is set to 'bencher', Criterion.rs will print output in a format that resembles the
    /// 'bencher' crate.
    #[argh(option, hidden_help, default = "OutputFormat::Criterion")]
    output_format: OutputFormat,
    /// ignored, but added for compatibility with libtest.
    #[argh(switch, hidden_help)]
    nocapture: bool,
    /// ignored, but added for compatibility with libtest.
    #[argh(switch, hidden_help)]
    show_output: bool,
    /// ignored, but added for compatibility with libtest.
    #[argh(switch, hidden_help)]
    include_ignored: bool,
    #[argh(switch, short = 'V', hidden_help)]
    version: bool,
}

impl Cli {
    fn validate(&self) -> Result<(), &'static str> {
        if self.quiet && self.verbose {
            return Err("--quite conflicts with --verbose");
        }
        if self.discard_baseline as u8
            + self.save_baseline.is_some() as u8
            + self.baseline.is_some() as u8
            + self.baseline_lenient.is_some() as u8
            > 1
        {
            return Err(
                "--discard-baseline, --save-baseline, --baseline nad --baseline-lenient conflicts with each other",
            );
        }
        if self.list as u8 + self.test as u8 + self.profile_time.is_some() as u8 > 1 {
            return Err("--list, --tests and --profile-time conflicts with each other");
        }
        if self.load_baseline.is_some() && self.profile_time.is_some() {
            return Err("--load-baseline conflicts with --profile-time");
        }
        if self.load_baseline.is_some() && self.baseline.is_none() {
            return Err("--load-baseline requires --baseline");
        }
        if self.quick && self.sample_size.is_some() {
            return Err("--quick conflicts with --sample-size");
        }
        Ok(())
    }
}

enum Color {
    Auto,
    Always,
    Never,
}

impl FromStr for Color {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "auto" => Self::Auto,
            "always" => Self::Always,
            "never" => Self::Never,
            _ => return Err(format!("invalid color: {s:?}")),
        })
    }
}

enum Format {
    Pretty,
    Terse,
}

impl FromStr for Format {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "pretty" => Self::Pretty,
            "terse" => Self::Terse,
            _ => return Err(format!("invalid format: {s:?}")),
        })
    }
}

enum PlottingBackend {
    Gnuplot,
    Plotters,
}

impl FromStr for PlottingBackend {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "gnuplut" => Self::Gnuplot,
            "plotters" => Self::Plotters,
            _ => return Err(format!("invalid plotting backend: {s:?}")),
        })
    }
}

enum OutputFormat {
    Criterion,
    Test,
}

impl FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "criterion" => Self::Criterion,
            "test" => Self::Test,
            _ => return Err(format!("invalid output format: {s:?}")),
        })
    }
}

fn main() -> Result<(), String> {
    let cli = argh::from_env::<Cli>();
    cli.validate()?;
    std::hint::black_box(cli);
    Ok(())
}

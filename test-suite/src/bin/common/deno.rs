// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.
//
// This file is trimmed from:
// <https://github.com/denoland/deno/pull/8617/files>
#![allow(dead_code)]
use super::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

static ENV_VARIABLES_HELP: &str = "ENVIRONMENT VARIABLES:
    DENO_DIR             Set the cache directory
    DENO_INSTALL_ROOT    Set deno install's output directory
                         (defaults to $HOME/.deno/bin)
    DENO_CERT            Load certificate authority from PEM encoded file
    NO_COLOR             Set to disable color
    HTTP_PROXY           Proxy address for HTTP requests
                         (module downloads, fetch)
    HTTPS_PROXY          Proxy address for HTTPS requests
                         (module downloads, fetch)
    NO_PROXY             Comma-separated list of hosts which do not use a proxy
                         (module downloads, fetch)";

static DENO_HELP: &str = "A secure JavaScript and TypeScript runtime

Docs: https://deno.land/manual
Modules: https://deno.land/std/ https://deno.land/x/
Bugs: https://github.com/denoland/deno/issues

To start the REPL:
  deno

To execute a script:
  deno run https://deno.land/std/examples/welcome.ts

To evaluate code in the shell:
  deno eval \"console.log(30933 + 404)\"
";

#[derive(Parser, Debug, Clone)]
#[command(
  name = "deno",
  after_long_help = ENV_VARIABLES_HELP,
  long_about = DENO_HELP,
)]
pub struct Opt {
    /// Enable unstable features and APIs
    #[arg(long, global = true)]
    unstable: bool,

    /// Set log level
    #[arg(long, short = 'L', global = true)]
    log_level: Option<String>,

    /// Suppress diagnostic output
    ///
    /// By default, subcommands print human-readable diagnostic messages to stderr.
    /// If the flag is set, restrict these messages to errors.
    #[arg(long, short, global = true)]
    quiet: bool,

    #[command(subcommand)]
    subcommand: Option<SubcommandEnum>,
}

#[derive(Subcommand, Debug, Clone)]
enum SubcommandEnum {
    Bundle(BundleSubcommand),
    Cache(CacheSubcommand),
    Compile(CompileSubcommand),
    Completions(CompletionsSubcommand),
    Doc(DocSubcommand),
    Eval(EvalSubcommand),
    Fmt(FmtSubcommand),
    Info(InfoSubcommand),
    Install(InstallSubcommand),
    Lint(LintSubcommand),
    Repl(ReplSubcommand),
    Run(RunSubcommand),
    Test(TestSubcommand),
    Types(TypesSubcommand),
    Upgrade(UpgradeSubcommand),
}

/// Bundle module and dependencies into single file
#[derive(Args, Debug, Clone)]
#[command(long_about = "Output a single JavaScript file with all dependencies.
  deno bundle https://deno.land/std/examples/colors.ts colors.bundle.js

If no output file is given, the output is written to standard output:
  deno bundle https://deno.land/std/examples/colors.ts")]
struct BundleSubcommand {
    source_file: String,

    out_file: Option<PathBuf>,

    #[command(flatten)]
    watch: WatchArg,

    #[command(flatten)]
    compilation: CompilationArgs,
}

/// Cache the dependencies
#[derive(Args, Debug, Clone)]
#[command(long_about = "Cache and compile remote dependencies recursively.

Download and compile a module with all of its static dependencies and save them
in the local cache, without running any code:
  deno cache https://deno.land/std/http/file_server.ts

Future runs of this module will trigger no downloads or compilation unless
--reload is specified.")]
struct CacheSubcommand {
    #[arg(required = true)]
    file: Vec<String>,

    #[command(flatten)]
    compilation: CompilationArgs,
}

/// Compiles the given script into a self contained executable.
///
///   deno compile --unstable https://deno.land/std/http/file_server.ts
///   deno compile --unstable --output /usr/local/bin/color_util https://deno.land/std/examples/colors.ts
/// The executable name is inferred by default:
///   - Attempt to take the file stem of the URL path. The above example would
///     become 'file_server'.
///   - If the file stem is something generic like 'main', 'mod', 'index' or 'cli',
///     and the path has no parent, take the file name of the parent path. Otherwise
///     settle with the generic name.
///   - If the resulting name has an '@...' suffix, strip it.
///
/// Cross compiling binaries for different platforms is not currently possible.
#[derive(Args, Debug, Clone)]
struct CompileSubcommand {
    source_file: String,

    /// Output file (defaults to $PWD/<inferred-name>)
    #[arg(long, short)]
    output: Option<PathBuf>,

    #[command(flatten)]
    compilation: CompilationArgs,
}

#[derive(ValueEnum, Debug, Clone)]
#[allow(clippy::enum_variant_names)]
enum Shell {
    Bash,
    Fish,
    Powershell,
    Zsh,
}

/// Generate shell completions
#[derive(Args, Debug, Clone)]
#[command(long_about = "Output shell completion script to standard output.
  deno completions bash > /usr/local/etc/bash_completion.d/deno.bash
  source /usr/local/etc/bash_completion.d/deno.bash")]
struct CompletionsSubcommand {
    shell: Shell,
}

/// Show documentation for a module.
///
/// Output documentation to standard output:
///     deno doc ./path/to/module.ts
///
/// Output private documentation to standard output:
///     deno doc --private ./path/to/module.ts
///
/// Output documentation in JSON format:
///     deno doc --json ./path/to/module.ts
///
/// Target a specific symbol:
///     deno doc ./path/to/module.ts MyClass.someField
///
/// Show documentation for runtime built-ins:
///     deno doc
///     deno doc --builtin Deno.Listener
#[derive(Args, Debug, Clone)]
struct DocSubcommand {
    /// Output documentation in JSON format
    #[arg(long)]
    json: bool,

    /// Output private documentation
    #[arg(long)]
    private: bool,

    source_file: Option<String>,

    /// Dot separated path to symbol
    #[arg(conflicts_with = "json")]
    filter: Option<String>,

    #[command(flatten)]
    import_map: ImportMapArg,

    #[command(flatten)]
    reload: ReloadArg,
}

/// Eval script
#[derive(Args, Debug, Clone)]
#[command(long_about = "Evaluate JavaScript from the command line.
  deno eval \"console.log('hello world')\"

To evaluate as TypeScript:
  deno eval -T \"const v: string = 'hello'; console.log(v)\"

This command has implicit access to all permissions (--allow-all).")]
struct EvalSubcommand {
    /// Treat eval input as TypeScript
    #[arg(long, short = 'T')]
    ts: bool,

    /// print result to stdout
    #[arg(long, short)]
    print: bool,

    /// Code arg
    #[arg(required = true, value_name = "CODE_ARG")]
    code_arg: Vec<String>,

    #[command(flatten)]
    runtime: RuntimeArgs,
}

/// Format source files
#[derive(Args, Debug, Clone)]
#[command(long_about = "Auto-format JavaScript/TypeScript source code.
  deno fmt
  deno fmt myfile1.ts myfile2.ts
  deno fmt --check

Format stdin and write to stdout:
  cat file.ts | deno fmt -

Ignore formatting code by preceding it with an ignore comment:
  // deno-fmt-ignore

Ignore formatting a file by adding an ignore comment at the top of the file:
  // deno-fmt-ignore-file")]
struct FmtSubcommand {
    /// Check if the source files are formatted
    #[arg(long)]
    check: bool,

    /// Ignore formatting particular source files. Use with --unstable
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    ignore: Vec<PathBuf>,

    files: Vec<PathBuf>,

    #[command(flatten)]
    watch: WatchArg,
}

/// Show info about cache or info related to source file
#[derive(Args, Debug, Clone)]
#[command(long_about = "Information about a module or the cache directories.

Get information about a module:
  deno info https://deno.land/std/http/file_server.ts

The following information is shown:

local: Local path of the file.
type: JavaScript, TypeScript, or JSON.
compiled: Local path of compiled source code. (TypeScript only.)
map: Local path of source map. (TypeScript only.)
deps: Dependency tree of the source file.

Without any additional arguments, 'deno info' shows:

DENO_DIR: Directory containing Deno-managed files.
Remote modules cache: Subdirectory containing downloaded remote modules.
TypeScript compiler cache: Subdirectory containing TS compiler output.")]
struct InfoSubcommand {
    file: Option<String>,

    /// Skip type checking modules
    #[arg(long, hide = true)]
    #[allow(dead_code)]
    no_check: bool,

    /// Outputs the information in JSON format
    #[arg(long)]
    json: bool,

    #[command(flatten)]
    import_map: ImportMapArg,

    #[command(flatten)]
    ca_file: CAFileArg,

    // duplicate arg: requires
    /// Reload source code cache (recompile TypeScript)
    ///
    /// --reload
    ///   Reload everything
    /// --reload=https://deno.land/std
    ///   Reload only standard modules
    /// --reload=https://deno.land/std/fs/utils.ts,https://deno.land/std/fmt/colors.ts
    ///   Reloads specific modules
    #[arg(
        long,
        short,
        use_value_delimiter = true,
        require_equals = true,
        value_name = "CACHE_BLOCKLIST",
        requires = "file"
    )]
    reload: Option<Vec<String>>,
}

/// Install script as an executable
#[derive(Args, Debug, Clone)]
#[command(
    long_about = "Installs a script as an executable in the installation root's bin directory.
  deno install --allow-net --allow-read https://deno.land/std/http/file_server.ts
  deno install https://deno.land/std/examples/colors.ts

To change the executable name, use -n/--name:
  deno install --allow-net --allow-read -n serve https://deno.land/std/http/file_server.ts

The executable name is inferred by default:
  - Attempt to take the file stem of the URL path. The above example would
    become 'file_server'.
  - If the file stem is something generic like 'main', 'mod', 'index' or 'cli',
    and the path has no parent, take the file name of the parent path. Otherwise
    settle with the generic name.
  - If the resulting name has an '@...' suffix, strip it.

To change the installation root, use --root:
  deno install --allow-net --allow-read --root /usr/local https://deno.land/std/http/file_server.ts

The installation root is determined, in order of precedence:
  - --root option
  - DENO_INSTALL_ROOT environment variable
  - $HOME/.deno

These must be added to the path manually if required."
)]
struct InstallSubcommand {
    #[arg(required = true, trailing_var_arg = true)]
    // TODO: allow_hyphen_values + trailing_var_args result in parsing unknown
    // `--options` as unnamed.
    cmd: Vec<String>,

    /// Executable file name
    #[arg(long, short)]
    name: Option<String>,

    /// Installation root
    #[arg(long)]
    root: Option<PathBuf>,

    /// Forcefully overwrite existing installation
    #[arg(long, short)]
    force: bool,

    #[command(flatten)]
    runtime: RuntimeArgs,

    #[command(flatten)]
    permissions: PermissionArgs,
}

/// Lint source files
#[derive(Args, Debug, Clone)]
#[command(long_about = "Lint JavaScript/TypeScript source code.
  deno lint --unstable
  deno lint --unstable myfile1.ts myfile2.js

Print result as JSON:
  deno lint --unstable --json

Read from stdin:
  cat file.ts | deno lint --unstable -
  cat file.ts | deno lint --unstable --json -

List available rules:
  deno lint --unstable --rules

Ignore diagnostics on the next line by preceding it with an ignore comment and
rule name:
  // deno-lint-ignore no-explicit-any

  // deno-lint-ignore require-await no-empty

Names of rules to ignore must be specified after ignore comment.

Ignore linting a file by adding an ignore comment at the top of the file:
  // deno-lint-ignore-file
")]
struct LintSubcommand {
    /// List available rules
    #[arg(long)]
    rules: bool,

    /// Ignore linting particular source files
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    // TODO: requires = "unstable"
    ignore: Vec<PathBuf>,

    /// Output lint result in JSON format
    #[arg(long)]
    json: bool,

    files: Vec<PathBuf>,
}

/// Read Eval Print Loop
#[derive(Args, Debug, Clone)]
struct ReplSubcommand {
    #[command(flatten)]
    runtime: RuntimeArgs,
}

/// Run a program given a filename or url to the module. Use '-' as a filename to read from stdin.
#[derive(Args, Debug, Clone)]
#[command(long_about = "Run a program given a filename or url to the module.

By default all programs are run in sandbox without access to disk, network or
ability to spawn subprocesses.
  deno run https://deno.land/std/examples/welcome.ts

Grant all permissions:
  deno run -A https://deno.land/std/http/file_server.ts

Grant permission to read from disk and listen to network:
  deno run --allow-read --allow-net https://deno.land/std/http/file_server.ts

Grant permission to read allow-listed files from disk:
  deno run --allow-read=/etc https://deno.land/std/http/file_server.ts

Deno allows specifying the filename '-' to read the file from stdin.
  curl https://deno.land/std/examples/welcome.ts | target/debug/deno run -")]
struct RunSubcommand {
    // duplicate arg: required
    // NOTE: these defaults are provided
    // so `deno run --v8-flags=--help` works
    // without specifying file to run.
    // TODO: default_value_ifs = [("v8-flags", "--help", "_"), ("v8-flags", "-help", "_")]
    /// Script arg
    #[arg(required = true, value_name = "SCRIPT_ARG")]
    script_arg: Vec<String>,

    // duplicate arg: conflicts
    /// Watch for file changes and restart process automatically
    #[arg(long)]
    // TODO: conflicts_with = "inspect",
    // TODO: conflicts_with = "inspect-brk"
    // TODO: requires = "unstable",
    watch: bool,

    #[command(flatten)]
    runtime: RuntimeArgs,

    #[command(flatten)]
    permissions: PermissionArgs,
}

/// Run tests
#[derive(Args, Debug, Clone)]
#[command(long_about = "Run tests using Deno's built-in test runner.

Evaluate the given modules, run all tests declared with 'Deno.test()' and
report results to standard output:
  deno test src/fetch_test.ts src/signal_test.ts

Directory arguments are expanded to all contained files matching the glob
{*_,*.,}test.{js,mjs,ts,jsx,tsx}:
  deno test src/")]
struct TestSubcommand {
    /// Cache test modules, but don't run tests
    #[arg(long)]
    // TODO: requires = "unstable"
    no_run: bool,

    /// Stop on first error
    #[arg(long, alias = "failfast")]
    fail_fast: bool,

    /// Don't return error code if no test files are found
    #[arg(long)]
    allow_none: bool,

    /// Run tests with this string or pattern in the test name
    #[arg(long, allow_hyphen_values = true)]
    filter: Option<String>,

    /// Collect coverage information
    #[arg(long)]
    // TODO: requires = "unstable", conflicts_with = "inspect", conflicts_with = "inspect"
    coverage: bool,

    /// List of file names to run
    files: Vec<String>,

    // TODO: arg(last)
    // duplicate arg: last
    // so `deno run --v8-flags=--help` works
    // without specifying file to run.
    // Script arg
    // #[arg(value_name = "SCRIPT_ARG", last = true, default_value_ifs = [
    // ("v8-flags", "--help", "_"),
    // ("v8-flags", "-help", "_"),
    // ])]
    // script_arg: Vec<String>,
    #[command(flatten)]
    runtime: RuntimeArgs,

    #[command(flatten)]
    permissions: PermissionArgs,
}

/// Print runtime TypeScript declarations.
#[derive(Args, Debug, Clone)]
#[command(long_about = "Print runtime TypeScript declarations.
  deno types > lib.deno.d.ts

The declaration file could be saved and used for typing information.")]
struct TypesSubcommand {}

/// Upgrade deno executable to given version
#[derive(Args, Debug, Clone)]
#[command(long_about = "Upgrade deno executable to the given version.
Defaults to latest.

The version is downloaded from
https://github.com/denoland/deno/releases
and is used to replace the current executable.

If you want to not replace the current Deno executable but instead download an
update to a different location, use the --output flag
  deno upgrade --output $HOME/my_deno")]
struct UpgradeSubcommand {
    /// The version to upgrade to
    #[arg(long)]
    version: Option<String>,

    /// The path to output the updated version to
    #[arg(long)]
    output: Option<PathBuf>,

    /// Perform all checks without replacing old exe
    #[arg(long)]
    dry_run: bool,

    /// Replace current exe even if not out-of-date
    #[arg(long, short)]
    force: bool,

    /// Upgrade to canary builds
    #[arg(long)]
    canary: bool,

    #[command(flatten)]
    ca_file: CAFileArg,
}

#[derive(Args, Debug, Clone)]
struct CompilationArgs {
    /// Do not resolve remote modules
    #[arg(long)]
    no_remote: bool,

    /// Load tsconfig.json configuration file
    #[arg(long, short, value_name = "FILE")]
    config: Option<String>,

    /// Skip type checking modules
    #[arg(long)]
    no_check: bool,

    /// Check the specified lock file
    #[arg(long, value_name = "FILE")]
    lock: Option<PathBuf>,

    /// Write lock file (use with --lock)
    #[arg(long, requires = "lock")]
    lock_write: bool,

    #[command(flatten)]
    import_map: ImportMapArg,

    #[command(flatten)]
    reload: ReloadArg,

    #[command(flatten)]
    ca_file: CAFileArg,
}

#[derive(Args, Debug, Clone)]
struct ImportMapArg {
    /// UNSTABLE: Load import map file
    #[arg(long, alias = "importmap", value_name = "FILE")]
    // TODO: requires = "unstable"
    import_map: Option<String>,
}

#[derive(Args, Debug, Clone)]
struct ReloadArg {
    /// Reload source code cache (recompile TypeScript)
    ///
    /// --reload
    ///   Reload everything
    /// --reload=https://deno.land/std
    ///   Reload only standard modules
    /// --reload=https://deno.land/std/fs/utils.ts,https://deno.land/std/fmt/colors.ts
    ///   Reloads specific modules
    #[arg(
        long,
        short,
        require_equals = true,
        use_value_delimiter = true,
        value_name = "CACHE_BLOCKLIST"
    )]
    reload: Option<Vec<String>>,
}

#[derive(Args, Debug, Clone)]
struct CAFileArg {
    /// Load certificate authority from PEM encoded file
    #[arg(long, value_name = "FILE")]
    cert: Option<String>,
}

#[derive(Args, Debug, Clone)]
struct PermissionArgs {
    /// Allow file system read access
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    allow_read: Option<Vec<PathBuf>>,

    /// Allow file system write access
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    allow_write: Option<Vec<PathBuf>>,

    /// Allow network access
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    allow_net: Option<Vec<String>>,

    /// Allow environment access
    #[arg(long)]
    allow_env: bool,

    /// Allow running subprocesses
    #[arg(long)]
    allow_run: bool,

    /// Allow loading plugins
    #[arg(long)]
    allow_plugin: bool,

    /// Allow high resolution time measurement
    #[arg(long)]
    allow_hrtime: bool,

    /// Allow all permissions
    #[arg(long, short = 'A')]
    allow_all: bool,
}

#[derive(Args, Debug, Clone)]
struct RuntimeArgs {
    /// activate inspector on host:port (default: 127.0.0.1:9229)
    #[arg(long, value_name = "HOST:PORT", require_equals = true)]
    inspect: Option<Option<String>>,

    /// activate inspector on host:port and break at start of user script
    #[arg(long, value_name = "HOST:PORT", require_equals = true)]
    inspect_brk: Option<Option<String>>,

    /// Require that remote dependencies are already cached
    #[arg(long)]
    cached_only: bool,

    /// Set V8 command line options (for help: --v8-flags=--help)
    #[arg(long, use_value_delimiter = true, require_equals = true)]
    v8_flags: Vec<String>,

    /// Seed Math.random()
    #[arg(long, value_name = "NUMBER")]
    seed: Option<u64>,

    #[command(flatten)]
    compilation: CompilationArgs,
}

#[derive(Args, Debug, Clone)]
struct WatchArg {
    /// Watch for file changes and restart process automatically
    #[arg(long)]
    // TODO: requires = "unstable"
    watch: bool,
}

# palc

[![crates.io](https://img.shields.io/crates/v/palc)](https://crates.io/crates/palc)
[![docs.rs](https://img.shields.io/docsrs/palc)](https://docs.rs/palc)

Prototype of a command line argument parser with several opposite design goals from [clap].

> ⚠️ This project is in alpha stage and is not ready for production yet.
> The API is subject to change. Feedbacks are welcome.

Check [`./examples/*-palc.rs`](./examples) for usages.

[clap]: https://github.com/clap-rs/clap

## Similar: Compatible[^1] Derive API

palc is an un-opinionated[^2] derive-based argument parser.
We choose to align with the clap 4.0 derive API: `Parser`, `Args` and `Subcommand`
macros with almost compatible `command(..)` and `arg(..)` attributes.

In most cases, switching between clap derive and palc derive is as easy as
changing a line in `Cargo.toml` and relevant `use` statements. No vendor locking.
Writing your CLI structs first before deciding which crate to use.

## Similar: Full-featured, out of the box

palc also aim to provide a decent CLI experience under default features:
help generations, non-UTF-8 support, argument constraints, `Args` composition,
subcommands, you name it.

Though some of clap features are not-yet-implemented.

<details>

<summary>Yet Implemented features</summary>

<!-- TODO: Document these in rustdoc, not here. -->

- Argument behaviors:
  - [x] Boolean flags `--verbose`.
  - [x] Named arguments `--long value`, `-svalue`
    - [x] Bundled short arguments `-czf`
    - [x] '='-separator `--long=v` `-f=v`.
    - [x] Aliases.
    - [x] Reject hyphen values.
    - [x] Allow hyphen values.
    - [ ] Space-delimited multi-values.
    - [x] Custom-delimited multi-values.
    - [ ] Multi-values with value-terminator.
  - [x] Unnamed/free/positional arguments `FILE`.
    - [x] Force no named arguments `--`.
    - [x] Greedy/tail arguments (`arg(trailing_var_arg)`).
    - [x] Last arguments after `--` (`arg(last)`).
    - [ ] Allow hyphen values.
  - [x] Counting number of occurrence.
  - [ ] Custom ArgAction.
  - [ ] Custom number of values (`arg(num_args)`).
  - [ ] Overrides.

  - List of [magic argument types](https://docs.rs/clap/4.5.40/clap/_derive/index.html#arg-types) with automatic default behaviors:
    - [x] `T where T: TryFrom<&OsStr> || TryFrom<&str> || FromStr` (named & unnamed)
    - [x] `bool` (named)
    - [x] `Option<T>` (named)
    - [ ] `Option<Option<T>>` (named)
      FIXME: The semantic disagrees with clap yet.
    - [x] `Vec<T>` (named & unnamed)
    - [x] `Option<Vec<T>>` (named & unnamed)
    - [ ] `Vec<Vec<T>>`
    - [ ] `Option<Vec<Vec<T>>>`
    
  - [x] Default values. (`arg(default_value_t)`)
    - [x] Default pre-parsed string value. (`arg(default_value)`)
      - Note: The provided string value will be parsed at runtime if the
        argument is missing. This will cause codegen degradation due to
        panic handling, and typos cannot be caught statically.
        Always use `arg(default_value_t)` if possible.
    - [ ] Default missing values.
    - [ ] Default from env.

- Argument value parsing:
  - [x] `derive(ValueEnum)`
    - [x] `value(rename_all)`
    - [x] `value(name)`
    - [ ] `value(skip)`
    - [ ] `value(help)`
  - [x] Non-UTF-8 inputs `PathBuf`, `OsString`.
  - [x] Automatically picked custom parser via `From<OsString>`, `From<String>` or `FromStr`.
  - [x] `arg(ignore_case)`
    - Note: Only `ValueEnum` that has no UPPERCASE variants are supported yet, due to implementation limitation.

- Argument validations:
  - [x] Reject duplicated arguments.
  - [x] Required.
    - [ ] Conditional required.
  - [x] Conflicts.
  - [x] Exclusive.
  - [ ] Args groups (one and only one argument).

- Composition:
  - [x] `arg(flatten)`.
    - Note that non-flatten arguments always take precedence over flatten arguments.
    - [x] Flatten named arguments.
    - [ ] Flatten unnamed arguments.
  - [x] Subcommands.
    - [ ] Argv0 as subcommand (multi-call binary).
    - [x] Prefer parsing subcommand over unnamed arguments.
    - [x] Global args.
      - Note: Current implementation has limitations on the number of values it takes.
        And it only propagates up if the inner Args cannot accept the named arguments --
        that is -- only one innermost Args on the ancestor chain will receive it, not all.

- [x] Help generation.
  Note: Help text is only for human consumption. The precise format is unstable,
  may change at any time and is not expected to exactly follow `clap`'s help format
  (although that is our general direction).

  - [x] Long help `--help`.
  - [ ] Short help `-h`.
  - [ ] Version `--version`.
  - [x] Custom header and footer.
  - [ ] Hiding.
  - [ ] Possible values of enums.
  - [x] Default values via `arg(default_value{,_t})`.
  - [ ] Custom help subcommand or flags.

- [ ] Helpful error messages.
  - [x] Error argument and reason.
  - [ ] Expected format.
  - [ ] Error suggestions ("did you mean").
  - [ ] Custom help template.

- Term features:
  - [ ] Colored output.
  - Wrap on terminal width.
    - We do not plan to implement this for now because its drawback outweighs
      its benefits. Word splitting and text rendering length with Unicode
      support is be very tricky and costly. It also hurts output reproducibility.

- [ ] Reflection.
- [ ] Completions.

</details>

## Different: Only via `derive` macros, statically

The only way to define a CLI parser in palc is via `derive`-macros. It is not
possible to manually write `impl` or even construct it dynamically.
Argument parsers are prepared, validated and generated during compile time.
The runtime does nothing other than parsing, thus has no startup overhead.
Also no insta-panics at runtime!

On the contrary, clap only works on builder API under the hood and its derive
API translates attributes to its builder API.  The parser is still composed,
verified, and then executed at runtime. This suffers from
[startup time penalty](https://github.com/clap-rs/clap/pull/4792).

This implies we do more work in proc-macro while rustc does less work on
generated code. In compilation time benchmarks, we outperform clap-derive in
both full build and incremental build.

## Different: Binary size aware

Despite how many features we have, we keep binary overhead in check.
Our goal is to give a size overhead that *deserves* its features, without
unreasonable or meaningless bloat.

Unlike other min-size-centric projects, eg. [pico-args] or [gumdrop], we choose
NOT to sacrifice CLI user experience, or force CLI designers to write more
(repetitive) code.
We are striving for a good balance between features and their cost.

In the benchmarks ([`./bench.txt`](./bench.txt)), binary size of small-to-medium
CLI structs using `palc` is comparable to and sometimes smaller than `argh`.

[pico-args]: https://crates.io/crates/pico-args
[gumdrop]: https://crates.io/crates/gumdrop

<br>

#### Credit

The derive interface is inspired and mimicking [`clap`][clap]'s derive interface.

The palc runtime design is inspired by [`miniserde`](https://github.com/dtolnay/miniserde).

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

[^1]: Due to design differences, some attributes cannot be implemented
statically or require a different syntax.
TODO: Document all attributes and notable differences with clap.

[^2]: [argh] say they are "opinionated" as an excuse of subjective and "creative" choice on derive attribute names and letter case restrictions. We are against these.

[argh]: https://github.com/google/argh

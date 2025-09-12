#!/usr/bin/env bash

filter="$1"
shift

die() {
    if [[ $# != 0 ]]; then
        echo "$*" >&2
    fi
    exit 1
}

getRequiredFeatures() {
    local name="$1"
    if [[ "$name" = *clap* ]]; then
        printf "%s" "--features clap "
    fi
    if [[ "$name" = *palc* ]]; then
        printf "%s" "--features palc "
    fi
    if [[ "$name" = *argh* ]]; then
        printf "%s" "--features argh "
    fi
}

getSize() {
    local -a cmd
    local out
    local name="$1"
    shift

    local cmd=(
        cargo bloated
        --package test-suite
        --bin "$name"
        --output sections
        --quiet \
        $(getRequiredFeatures "$name")
        "$@"
        -- --quiet 
    )

    out="$("${cmd[@]}")" || die "command fail: ${cmd[*]}"
    sed -nE 's/.*\s(\S+)\s+\(file\).*/\1/p' <<<"$out"
}

getCompileTime() {
    local name="$1"
    shift

    local cmd=(
        cargo build
        --package test-suite
        --bin "$name"
        --quiet
        $(getRequiredFeatures "$name")
        "$@"
    )
    # %E: elapsed time.
    CARGO_TARGET_DIR="$tmpdir" \
        command time --format "%E" -- \
        "${cmd[@]}" 2>&1 || die "failed to build"
}


tmpdir="$(mktemp -d /tmp/palc-target.XXXXXX)" || die "failed to mktemp"
trap 'rm -r -- "$tmpdir"' EXIT

# Prepare and download dependencies sources.
cargo metadata --format-version 1 >/dev/null || die "failed to run cargo metadata"

printf "%-20s %12s %12s %12s %12s\n" "name" "minimal" "default" "full-build" "incremental"

for name in simple-{clap,argh,palc,none} criterion-{clap,argh,palc} deno-{clap,palc}; do
    if [[ "$name" != *"$filter"* ]]; then
        continue
    fi

    minSize="$(getSize $name)" || die
    defaultSize="$(getSize $name --features full-featured)" || die
    printf "%-20s %12s %12s" "$name" "$minSize" "$defaultSize"

    rm -rf "$tmpdir/debug"
    buildTime="$(getCompileTime "$name" --features full-featured)" || die
    incTime="$(getCompileTime "$name" --features full-featured)" || die
    printf "%12s %12s\n" "$buildTime" "$incTime"
done

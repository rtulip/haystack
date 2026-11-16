#!/usr/bin/env bash
set -e

cargo install cargo-llvm-cov

(
    set -e

    export CARGO_TARGET_DIR=target/coverage
    source <(cargo llvm-cov show-env --sh)

    cargo llvm-cov clean --workspace
    cargo build
    cargo test
    cargo llvm-cov report --html
)
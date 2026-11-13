# Configure instrumentation and coverage environment
source <(cargo llvm-cov show-env --sh)

# Clean old build artifacts
cargo llvm-cov clean --workspace

# Build the compiler with instrumentation
cargo build

# Run integration tests, which invoke the compiler
cargo test

# Generate the coverage report
cargo llvm-cov report --html
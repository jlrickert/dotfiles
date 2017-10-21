export CARGO_HOME=$HOME/.cargo
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

path=(
    $HOME/.cargo/bin
    $path[@]
)

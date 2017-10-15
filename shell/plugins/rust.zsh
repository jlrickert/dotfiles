export CARGO_HOME=$HOME/.cargo
export RUST_SRC_PATH=~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src

path=(
    $HOME/.cargo/bin
    $path[@]
)

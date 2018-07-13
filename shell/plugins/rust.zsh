RUSTC_BIN=(which rustc)
if [[ ${?} -eq 0 ]]; then
    export CARGO_HOME=$HOME/.cargo
    export RUST_SRC_PATH="$($RUSTC_BIN --print sysroot)/lib/rustlib/src/rust/src"

    path=(
        $HOME/.cargo/bin
        $path[@]
    )
fi

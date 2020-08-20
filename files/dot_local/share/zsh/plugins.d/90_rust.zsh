RUSTC_BIN=(which rustc)
if [[ ${?} -eq 0 ]]; then
	export CARGO_HOME=$XDG_DATA_HOME/cargo
	export RUST_SRC_PATH="$($RUSTC_BIN --print sysroot)/lib/rustlib/src/rust/src"

	path=(
		$XDG_DATA_HOME/cargo/bin
		$path[@]
	)
fi

# source /home/jlrickert/.local/share/cargo/env
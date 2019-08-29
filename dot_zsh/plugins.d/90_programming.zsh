############################################################
# Golang config
############################################################
export GOPATH=$HOME/golang

path=(
    $GOPATH/bin
    $path[@]
)

############################################################
# Javascript config
############################################################
export npm_config_prefix=~/.node_modules

path=(
    $HOME/.node_modules/bin
    $path[@]
)


############################################################
# Rust config
############################################################
RUSTC_BIN=(which rustc)
if [[ ${?} -eq 0 ]]; then
  export CARGO_HOME=$HOME/.cargo
  export RUST_SRC_PATH="$($RUSTC_BIN --print sysroot)/lib/rustlib/src/rust/src"

  path=(
    $HOME/.cargo/bin
    $path[@]
  )
fi

############################################################
# Ruby config
############################################################
path=(
  $HOME/.gem/ruby/2.5.0/bin
)

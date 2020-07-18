############################################################
# Golang config
############################################################
export GOPATH=$HOME/.local/share/golang
export GO111MODULE=on 

path=(
    $GOPATH/bin
    $path[@]
)

############################################################
# Javascript config
############################################################
export NVM_DIR="$XDG_DATA_HOME/nvm"
#export npm_config_prefix=$XDG_DATA_HOME/node_modules # breaks nvm

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

path=(
    #$npm_config_prefix/bin # depends on line 15 to work
    $HOME/.node_modules/bin
    $path[@]
)

############################################################
# Rust config
############################################################
RUSTC_BIN=(which rustc)
if [[ ${?} -eq 0 ]]; then
  export CARGO_HOME=$XDG_DATA_HOME/cargo
  export RUST_SRC_PATH="$($RUSTC_BIN --print sysroot)/lib/rustlib/src/rust/src"

  path=(
    $XDG_DATA_HOME/cargo/bin
    $path[@]
  )
fi

############################################################
# Ruby config
############################################################
path=(
  $HOME/.gem/ruby/2.5.0/bin
  $path[@]
)

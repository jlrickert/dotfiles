export NVM_DIR="$XDG_DATA_HOME/nvm"
# FIXME: get npm crap out of root home directory; cache or share dir better?
#export npm_config_prefix=$XDG_DATA_HOME/node_modules # breaks nvm

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

export DENO_INSTALL="$XDG_DATA_HOME/.deno"

path=(
	#$npm_config_prefix/bin # depends on npm_config_prefix working
	$HOME/.node_modules/bin
	$DENO_INSTALL/bin
	$path[@]
)

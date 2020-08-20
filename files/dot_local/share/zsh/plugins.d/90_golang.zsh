export GOPATH=$XDG_DATA_HOME/go
export GO111MODULE=on 

path=(
	$GOPATH/bin
	$path[@]
)

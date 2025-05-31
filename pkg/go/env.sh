GITUSER="${USER:-$(whoami)}"
export GOPRIVATE="github.com/${GITUSER}/*,gitlab.com/${GITUSER}/*"
export GOPATH="${XDG_DATA_HOME:-$(whoami)/.local/share}/go"
export GOBIN="${HOME}/.local/go/bin"
export GOPROXY=direct
export CGO_ENABLED=0

pathappend "${GOBIN}"

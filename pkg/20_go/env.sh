GITUSER="${USER:-$(whoami)}"
export GOPRIVATE="github.com/${GITUSER}/*,gitlab.com/${GITUSER}/*"
export GOROOT="${XDG_DATA_HOME:-$(whoami)/.local/share}/go"
export GOBIN="${HOME}/.local/bin"
export GOPROXY=direct
export CGO_ENABLED=0

pathprepend "${GOROOT}/bin"

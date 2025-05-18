if have /opt/homebrew/bin/brew; then
	# shellcheck source=/opt/homebrew/bin/brew
	source <(/opt/homebrew/bin/brew shellenv)

	python_bin="$(find /Users/jlrickert/Library/Python -name "bin")"
	[[ -d "${python_bin}" ]] && _pathappend "${python_bin}"

	pathappend /opt/homebrew/opt/mysql-client/bin
fi

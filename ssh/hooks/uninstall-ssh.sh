#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

BEGIN_MARKER="# BEGIN dotfiles-ssh"
END_MARKER="# END dotfiles-ssh"

strip_stub() {
	local target="$1" tmp
	[ -f "${target}" ] || return 0
	grep -qF "${BEGIN_MARKER}" "${target}" || return 0

	tmp="$(mktemp)"
	awk -v begin="${BEGIN_MARKER}" -v end="${END_MARKER}" '
		$0 == begin { in_block=1; next }
		$0 == end && in_block { in_block=0; next }
		!in_block { print }
	' "${target}" >"${tmp}"
	mv "${tmp}" "${target}"
	echo "ssh: stripped dotfiles marker from ${target}"
}

strip_stub "${HOME}/.ssh/config"

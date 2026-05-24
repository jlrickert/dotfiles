#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

BEGIN_MARKER="# BEGIN dotfiles-ssh"
END_MARKER="# END dotfiles-ssh"

render_block() {
	cat <<EOF
${BEGIN_MARKER}
# Managed by dotfiles. The Include below pulls in defaults from
# ~/.config/ssh/managed.conf. Host entries below this block are user-maintained.
Include ~/.config/ssh/managed.conf
${END_MARKER}
EOF
}

inject_stub() {
	local target="$1" block tmp
	block="$(render_block)"

	if [ ! -f "${target}" ]; then
		printf '%s\n' "${block}" >"${target}"
		echo "ssh: created ${target} with dotfiles marker"
		return 0
	fi

	if grep -qF "${BEGIN_MARKER}" "${target}"; then
		tmp="$(mktemp)"
		BLOCK="${block}" awk \
			-v begin="${BEGIN_MARKER}" \
			-v end="${END_MARKER}" '
			$0 == begin { print ENVIRON["BLOCK"]; in_block=1; next }
			$0 == end && in_block { in_block=0; next }
			!in_block { print }
		' "${target}" >"${tmp}"
		mv "${tmp}" "${target}"
		echo "ssh: refreshed dotfiles marker in ${target}"
	else
		# ssh_config gives the FIRST matching value precedence, so the Include
		# (which pulls in Host * defaults) must land above user-maintained host
		# entries to take effect uniformly.
		tmp="$(mktemp)"
		{ printf '%s\n\n' "${block}"; cat "${target}"; } >"${tmp}"
		mv "${tmp}" "${target}"
		echo "ssh: prepended dotfiles marker to ${target}"
	fi
}

mkdir -p "${HOME}/.ssh"
chmod 700 "${HOME}/.ssh"
inject_stub "${HOME}/.ssh/config"
chmod 600 "${HOME}/.ssh/config"

# managed.conf is installed by dots into ~/.config/ssh/managed.conf; chmod
# it 600 too so it can never be looser than the ~/.ssh/config that Includes it.
managed="${XDG_CONFIG_HOME:-$HOME/.config}/ssh/managed.conf"
[ -f "${managed}" ] && chmod 600 "${managed}"

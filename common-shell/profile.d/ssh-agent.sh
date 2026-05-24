# ssh-agent fallback for non-macOS. macOS launchd already exports
# SSH_AUTH_SOCK; this block is a no-op there. On Linux, reuse a cached
# agent across shell sessions instead of spawning one per terminal.
if [ -z "${SSH_AUTH_SOCK:-}" ] && command -v ssh-agent >/dev/null 2>&1; then
	_ssh_agent_env="${XDG_RUNTIME_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}}/ssh-agent.env"
	[ -r "${_ssh_agent_env}" ] && . "${_ssh_agent_env}" >/dev/null
	if ! ssh-add -l >/dev/null 2>&1; then
		mkdir -p "$(dirname "${_ssh_agent_env}")"
		ssh-agent -s >"${_ssh_agent_env}"
		. "${_ssh_agent_env}" >/dev/null
	fi
	unset _ssh_agent_env
fi

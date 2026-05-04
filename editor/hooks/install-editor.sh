#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Run a command as root when not already root. apt + add-apt-repository
# need root; brew install does not. Defined inline so the hook does not
# depend on any framework helper from the legacy pkg/ tree.
run_as_root() {
	if [ "$(id -u)" -eq 0 ]; then
		"$@"
	elif command -v sudo >/dev/null 2>&1; then
		sudo "$@"
	else
		echo "editor: need root for: $*; install sudo or run as root." >&2
		return 1
	fi
}

# --- 1. Ensure neovim is installed ---
# darwin → brew (assumed present, hard-fail if not).
# debian/ubuntu → upstream release tarball pinned to NVIM_VERSION. The PPA
# (neovim-ppa/unstable) ships builds older than what jlrickert/kickstart.nvim
# targets, so we bypass apt entirely on Linux and install the static binary
# under /usr/local. Pin matches the user's macOS Homebrew nvim so kickstart
# behaves identically across hosts.
# everything else → warn and skip; user can install manually.
NVIM_VERSION="0.12.2"

# Compare two dotted version strings ("0.12.2"). Returns 0 iff $1 >= $2.
# Pure shell — sort -V is GNU coreutils and present on every supported host
# (macOS ships a BSD sort that lacks -V; we only call this on Linux).
nvim_version_ge() {
	[ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n1)" = "$2" ]
}

nvim_needs_install=1
if command -v nvim >/dev/null 2>&1; then
	# nvim --version → "NVIM v0.9.5\n..." on the PPA build, "NVIM v0.12.2\n..."
	# on upstream. Strip the leading "v" so nvim_version_ge gets a clean
	# dotted triple to compare against NVIM_VERSION.
	nvim_current="$(nvim --version 2>/dev/null | head -n1 | awk '{print $2}' | sed 's/^v//')"
	if [ -n "${nvim_current}" ] && nvim_version_ge "${nvim_current}" "${NVIM_VERSION}"; then
		echo "editor: nvim ${nvim_current} already installed at $(command -v nvim) (>= ${NVIM_VERSION})."
		nvim_needs_install=0
	else
		echo "editor: nvim ${nvim_current:-unknown} below floor ${NVIM_VERSION}; reinstalling."
	fi
fi

if [ "${nvim_needs_install}" -eq 1 ]; then
	case "$(uname -s)" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "editor: Homebrew not found on macOS; install brew first." >&2
			exit 1
		fi
		echo "editor: installing neovim via brew"
		brew install neovim
		;;
	Linux)
		if [ -r /etc/os-release ]; then
			# shellcheck disable=SC1091
			. /etc/os-release
		else
			ID=""
		fi
		case "${ID:-}" in
		debian | ubuntu)
			nvim_arch=""
			case "$(uname -m)" in
			x86_64) nvim_arch=x86_64 ;;
			aarch64 | arm64) nvim_arch=arm64 ;;
			*)
				echo "editor: unsupported arch '$(uname -m)' for upstream nvim tarball; skipping." >&2
				;;
			esac
			if [ -n "${nvim_arch}" ]; then
				# Best-effort uninstall of any apt-managed nvim (PPA leftover
				# from older bootstraps). Don't fail the hook if the package
				# isn't installed -- that's the steady state on fresh images.
				run_as_root apt-get -y remove neovim neovim-runtime >/dev/null 2>&1 || true
				nvim_tmp="$(mktemp -d)"
				nvim_tarball="nvim-linux-${nvim_arch}.tar.gz"
				nvim_url="https://github.com/neovim/neovim/releases/download/v${NVIM_VERSION}/${nvim_tarball}"
				echo "editor: downloading neovim v${NVIM_VERSION} (${nvim_arch})"
				if curl -fsSL "${nvim_url}" -o "${nvim_tmp}/${nvim_tarball}"; then
					tar -xzf "${nvim_tmp}/${nvim_tarball}" -C "${nvim_tmp}"
					# Tarball top-level is nvim-linux-<arch>/; copy bin/, lib/,
					# share/ contents into /usr/local so the runtime tree
					# (parser libs, doc, plugin/) lands next to the binary.
					nvim_extracted="${nvim_tmp}/nvim-linux-${nvim_arch}"
					if [ -d "${nvim_extracted}" ]; then
						run_as_root cp -R "${nvim_extracted}/bin/." /usr/local/bin/
						[ -d "${nvim_extracted}/lib" ] && run_as_root cp -R "${nvim_extracted}/lib/." /usr/local/lib/
						[ -d "${nvim_extracted}/share" ] && run_as_root cp -R "${nvim_extracted}/share/." /usr/local/share/
					else
						echo "editor: unexpected tarball layout (no ${nvim_extracted}); skipping." >&2
					fi
				else
					echo "editor: neovim download failed; skipping." >&2
				fi
				rm -rf "${nvim_tmp}"
			fi
			;;
		*)
			echo "editor: no automatic neovim install path on '${ID:-unknown}'; install manually." >&2
			;;
		esac
		;;
	*)
		echo "editor: unsupported OS '$(uname -s)'; install neovim manually." >&2
		;;
	esac
fi

# --- 2. Sync kickstart.nvim into ~/.config/nvim ---
# Only manage the directory if it doesn't exist or is already our fork.
# Don't clobber a user's hand-rolled config; warn and continue.
KICKSTART_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/nvim"
KICKSTART_REPO="https://github.com/jlrickert/kickstart.nvim"

if [ -d "${KICKSTART_DIR}/.git" ]; then
	if git -C "${KICKSTART_DIR}" remote get-url origin 2>/dev/null | grep -q "jlrickert/kickstart.nvim"; then
		echo "editor: refreshing kickstart.nvim in ${KICKSTART_DIR}"
		# Best-effort pull. Don't tank the install on a dirty tree or
		# offline machine.
		git -C "${KICKSTART_DIR}" pull --ff-only || echo "editor: kickstart.nvim pull failed; leaving alone."
	else
		echo "editor: ${KICKSTART_DIR} is a git repo but not jlrickert/kickstart.nvim; leaving alone."
	fi
elif [ ! -e "${KICKSTART_DIR}" ]; then
	echo "editor: cloning ${KICKSTART_REPO} into ${KICKSTART_DIR}"
	mkdir -p "$(dirname "${KICKSTART_DIR}")"
	git clone "${KICKSTART_REPO}" "${KICKSTART_DIR}"
else
	echo "editor: ${KICKSTART_DIR} exists but is not a git checkout; leaving alone."
fi

# --- 3. cmd-edit (jlrickert/cmd-edit, binary name `ed`) ---
# darwin → brew tap formula (also installs shell completions).
# linux  → upstream tarball release; binary lands at /usr/local/bin/ed.
# Path-based idempotent skip on linux because the system ships a /usr/bin/ed
# (classic line editor) that would shadow a `command -v ed` check.
CMD_EDIT_REPO="jlrickert/cmd-edit"

case "$(uname -s)" in
Darwin)
	if command -v ed >/dev/null 2>&1 && ed --version >/dev/null 2>&1; then
		echo "editor: cmd-edit already installed at $(command -v ed)."
	elif ! command -v brew >/dev/null 2>&1; then
		echo "editor: brew missing; skipping cmd-edit." >&2
	else
		echo "editor: installing jlrickert/formulae/cmd-edit via brew"
		brew install jlrickert/formulae/cmd-edit
	fi
	;;
Linux)
	if [ -x /usr/local/bin/ed ] && /usr/local/bin/ed --version >/dev/null 2>&1; then
		echo "editor: cmd-edit already installed at /usr/local/bin/ed."
	else
		ed_arch=""
		case "$(uname -m)" in
		x86_64) ed_arch=amd64 ;;
		aarch64 | arm64) ed_arch=arm64 ;;
		*)
			echo "editor: unsupported arch '$(uname -m)' for cmd-edit; skipping." >&2
			;;
		esac
		if [ -n "${ed_arch}" ]; then
			# Resolve the latest release tag via the redirect target of
			# /releases/latest. Pure curl + sed; no jq dependency.
			ed_version=$(curl -fsSLI -o /dev/null -w '%{url_effective}' \
				"https://github.com/${CMD_EDIT_REPO}/releases/latest" |
				sed 's|.*/tag/v||' | tr -d '\r\n')
			if [ -z "${ed_version}" ]; then
				echo "editor: could not resolve latest cmd-edit version; skipping." >&2
			else
				ed_tmp=$(mktemp -d)
				ed_url="https://github.com/${CMD_EDIT_REPO}/releases/download/v${ed_version}/ed_${ed_version}_linux_${ed_arch}.tar.gz"
				echo "editor: downloading cmd-edit v${ed_version} (${ed_arch})"
				if curl -fsSL "${ed_url}" -o "${ed_tmp}/ed.tar.gz"; then
					tar -xzf "${ed_tmp}/ed.tar.gz" -C "${ed_tmp}"
					run_as_root install -m 0755 "${ed_tmp}/ed" /usr/local/bin/ed
				else
					echo "editor: cmd-edit download failed; skipping." >&2
				fi
				rm -rf "${ed_tmp}"
			fi
		fi
	fi
	;;
*)
	echo "editor: cmd-edit unsupported on '$(uname -s)'; skipping."
	;;
esac

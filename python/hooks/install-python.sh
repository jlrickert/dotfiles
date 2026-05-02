#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Resolve the OS once so both the python and uv sections share the same
# distro gate. Each tool is then installed in its own section with its own
# idempotent skip block -- a partial failure (python ok, uv broken, or
# vice versa) leaves a diagnosable trail rather than one fused step.
OS="$(uname -s)"

# On Linux, also resolve the distro family up-front. Both sections need it.
DISTRO_OK=0
if [ "${OS}" = Linux ]; then
	if [ ! -r /etc/os-release ]; then
		echo "python: cannot identify Linux distribution from /etc/os-release." >&2
		exit 1
	fi
	# shellcheck disable=SC1091
	. /etc/os-release
	case "${ID:-}" in
	debian | ubuntu) DISTRO_OK=1 ;;
	*)
		case "${ID_LIKE:-}" in
		*debian*) DISTRO_OK=1 ;;
		esac
		;;
	esac
	if [ "${DISTRO_OK}" -ne 1 ]; then
		echo "python: unsupported Linux distro '${ID:-unknown}'; install python3 and uv manually." >&2
		exit 0
	fi

	# Sudo-free when EUID=0 (matches the docker build path running as root);
	# otherwise prefix sudo so a developer-machine install still works.
	SUDO=""
	if [ "${EUID:-$(id -u)}" -ne 0 ]; then
		SUDO="sudo"
	fi
fi

# ---------------------------------------------------------------------------
# Section 1: Python
# ---------------------------------------------------------------------------
# Idempotent skip: if python3 is already on PATH and meets the >=3.11 floor
# we declare for dots scripting, leave it alone. Mirrors the bun/rust pattern
# of deferring to any pre-existing install rather than clobbering it.
if command -v python3 >/dev/null 2>&1 \
	&& python3 -c 'import sys; sys.exit(0 if sys.version_info >= (3, 11) else 1)' >/dev/null 2>&1; then
	echo "python: python3 already present (>=3.11) at $(command -v python3); skipping."
else
	case "${OS}" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "python: Homebrew not found on macOS; install brew first." >&2
			exit 1
		fi
		# Unversioned formula: brew's `python` currently aliases to the
		# latest stable (3.13 today) and rolls forward as brew updates.
		# Pinning python@3.13 would defeat that.
		echo "python: installing via brew install python"
		brew install python
		;;
	Linux)
		echo "python: installing via apt-get install python3 python3-pip"
		${SUDO} apt-get update
		${SUDO} apt-get install -y python3 python3-pip
		;;
	*)
		echo "python: unsupported OS '${OS}'; install manually." >&2
		exit 0
		;;
	esac
fi

# ---------------------------------------------------------------------------
# Section 2: uv (Astral's Python package/project manager)
# ---------------------------------------------------------------------------
# Separate brew install line on darwin (not chained with python above) so a
# partial failure is easier to diagnose. On Linux there's no maintained apt
# package for uv, so we use Astral's official installer.
if command -v uv >/dev/null 2>&1; then
	echo "python: uv already present at $(command -v uv); skipping."
else
	case "${OS}" in
	Darwin)
		if ! command -v brew >/dev/null 2>&1; then
			echo "python: Homebrew not found on macOS; install brew first." >&2
			exit 1
		fi
		echo "python: installing uv via brew install uv"
		brew install uv
		;;
	Linux)
		# Default install dir is ~/.local/bin (or $XDG_BIN_HOME). When EUID=0
		# (Docker test image), ~/.local/bin resolves to /root/.local/bin,
		# which is NOT on the test user's $PATH -- the verify script asserts
		# the user's ~/.local/bin. Override to /usr/local/bin so uv lands on
		# a system path visible to every user. When EUID!=0, the default
		# ~/.local/bin is correct for a developer-machine install.
		if [ "${EUID:-$(id -u)}" -eq 0 ]; then
			echo "python: installing uv via Astral installer (UV_INSTALL_DIR=/usr/local/bin, root)"
			curl -LsSf https://astral.sh/uv/install.sh | env UV_INSTALL_DIR=/usr/local/bin sh
		else
			echo "python: installing uv via Astral installer (default ~/.local/bin)"
			curl -LsSf https://astral.sh/uv/install.sh | sh
		fi
		;;
	*)
		echo "python: unsupported OS '${OS}'; install uv manually." >&2
		exit 0
		;;
	esac
fi

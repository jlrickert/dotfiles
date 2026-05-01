#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Resolve package root from the hook's own location: hooks/install-codex.sh
# lives one directory below the package root. The legacy script depended on
# PACKAGE_ROOT exported by the dotsh framework; this version stands alone.
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
package_root="$(cd "${script_dir}/.." && pwd)"
skills_source_root="${package_root}/skills"
codex_home="${CODEX_HOME:-${HOME}/.codex}"
skills_target_root="${codex_home}/skills"

# --- 1. Install supporting brew formulae if brew is present ---
# python+pipx are convenience tools the legacy script installed alongside
# codex. Skip silently if brew is unavailable so non-darwin (or brewless)
# installs aren't blocked.
if command -v brew >/dev/null 2>&1; then
	for formula in python pipx; do
		if brew list --formula "${formula}" >/dev/null 2>&1; then
			echo "codex: brew formula '${formula}' already installed."
			continue
		fi
		echo "codex: installing brew formula '${formula}'"
		brew install "${formula}"
	done
else
	echo "codex: brew not found; skipping python/pipx prerequisites."
fi

# --- 2. Install codex itself, dual fallback ---
# Preferred: brew cask on darwin (matches user's package manager). Fallback:
# npm global. If neither path lands codex on PATH, fail loudly.
if command -v codex >/dev/null 2>&1; then
	echo "codex: already installed at $(command -v codex)."
else
	if command -v brew >/dev/null 2>&1; then
		echo "codex: attempting brew install --cask codex"
		if ! brew install --cask codex; then
			echo "codex: brew cask install failed; will try npm next." >&2
		fi
	fi

	if ! command -v codex >/dev/null 2>&1 && command -v npm >/dev/null 2>&1; then
		echo "codex: attempting npm install -g @openai/codex"
		npm install -g @openai/codex
	fi

	if ! command -v codex >/dev/null 2>&1; then
		echo "codex: unable to install automatically; install Homebrew or npm first." >&2
		exit 1
	fi
fi

# --- 3. Symlink packaged SKILL.md trees into ~/.codex/skills/ ---
# Codex discovers skills under ~/.codex/skills. The legacy install walked
# the package's skills/ tree for SKILL.md and symlinked each containing
# directory. Same shape here so the user-visible layout doesn't change.
if [ ! -d "${skills_source_root}" ]; then
	echo "codex: no packaged skills at '${skills_source_root}'; nothing to link."
	exit 0
fi

mkdir -p "${skills_target_root}"

# Sweep dangling symlinks left by removed skills before relinking.
for link in "${skills_target_root}"/*; do
	if [ -L "${link}" ] && [ ! -e "${link}" ]; then
		echo "codex: removing dangling skill symlink '$(basename "${link}")'"
		rm -f "${link}"
	fi
done

skills_count=0
while IFS= read -r -d '' skill_file; do
	skills_count=$((skills_count + 1))
	skill_dir="$(dirname "${skill_file}")"
	relative_skill_path="${skill_dir#"${skills_source_root}/"}"
	target_skill_dir="${skills_target_root}/${relative_skill_path}"
	target_parent_dir="$(dirname "${target_skill_dir}")"

	mkdir -p "${target_parent_dir}"

	if [ -L "${target_skill_dir}" ]; then
		current_link_target="$(readlink "${target_skill_dir}")"
		if [ "${current_link_target}" = "${skill_dir}" ]; then
			echo "codex: skill already linked '${relative_skill_path}'"
			continue
		fi
		rm -f "${target_skill_dir}"
	fi

	ln -s "${skill_dir}" "${target_skill_dir}"
	echo "codex: linked skill '${relative_skill_path}'"
done < <(find "${skills_source_root}" -type f -name SKILL.md -print0)

if [ "${skills_count}" -eq 0 ]; then
	echo "codex: no SKILL.md files found under '${skills_source_root}'; nothing to link."
else
	echo "codex: linked ${skills_count} skill(s) into '${skills_target_root}'."
fi

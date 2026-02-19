#!/usr/bin/env bash

ensure_environment

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
skills_source_root="${PACKAGE_ROOT:-${script_dir}}/skills"
codex_home="${CODEX_HOME:-${HOME}/.codex}"
skills_target_root="${codex_home}/skills"

if have codex; then
	log_message INFO "Codex is already installed."
else
	if have brew; then
		log_message INFO "Attempting to install Codex with Homebrew cask..."
		if brew install --cask codex; then
			log_message SUCCESS "Codex installed successfully with Homebrew."
		else
			log_message WARN "Homebrew install failed. Falling back to npm."
		fi
	fi

	if ! have codex && have npm; then
		log_message INFO "Attempting to install Codex with npm..."
		if npm install -g @openai/codex; then
			log_message SUCCESS "Codex installed successfully with npm."
		else
			log_message ERROR "Failed to install Codex with npm." >&2
			exit 1
		fi
	fi

	if ! have codex; then
		log_message ERROR "Unable to install Codex automatically: install Homebrew or npm first." >&2
		exit 1
	fi
fi

if [ ! -d "${skills_source_root}" ]; then
	log_message WARN "No packaged Codex skills found at \`${skills_source_root}\`. Skipping skill symlinks."
	exit 0
fi

mkdir -p "${skills_target_root}"

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
			log_message INFO "Skill already linked: \`${relative_skill_path}\`"
			continue
		fi
		rm -f "${target_skill_dir}"
	fi

	ln -s "${skill_dir}" "${target_skill_dir}"
	log_message SUCCESS "Linked Codex skill: \`${relative_skill_path}\`"
done < <(find "${skills_source_root}" -type f -name SKILL.md -print0)

if [ "${skills_count}" -eq 0 ]; then
	log_message WARN "No SKILL.md files found under \`${skills_source_root}\`. Nothing to link."
else
	log_message SUCCESS "Linked ${skills_count} Codex skill(s) into \`${skills_target_root}\`."
fi

#!/usr/bin/env bash

ensure_environment

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
skills_source_root="${PACKAGE_ROOT:-${script_dir}}/skills"
agents_source_root="${PACKAGE_ROOT:-${script_dir}}/agents"
claude_home="${CLAUDE_HOME:-${HOME}/.claude}"
skills_target_root="${claude_home}/skills"
agents_target_root="${claude_home}/agents"

if have claude; then
	log_message INFO "Claude is already installed."
else
	if have brew; then
		log_message INFO "Installing Claude with Homebrew..."
		if brew install claude-code; then
			log_message SUCCESS "Claude installed successfully with Homebrew."
		else
			log_message ERROR "Failed to install Claude with Homebrew." >&2
			exit 1
		fi
	else
		log_message ERROR "Homebrew required to install Claude." >&2
		exit 1
	fi
fi

if [ -d "${skills_source_root}" ]; then
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
		log_message SUCCESS "Linked Claude skill: \`${relative_skill_path}\`"
	done < <(find "${skills_source_root}" -type f -name SKILL.md -print0)

	if [ "${skills_count}" -eq 0 ]; then
		log_message WARN "No SKILL.md files found under \`${skills_source_root}\`. Nothing to link."
	else
		log_message SUCCESS "Linked ${skills_count} Claude skill(s) into \`${skills_target_root}\`."
	fi
else
	log_message WARN "No packaged Claude skills found at \`${skills_source_root}\`. Skipping skill symlinks."
fi

if [ -d "${agents_source_root}" ]; then
	mkdir -p "${agents_target_root}"

	agents_count=0
	while IFS= read -r -d '' agent_file; do
		agents_count=$((agents_count + 1))
		relative_agent_path="${agent_file#"${agents_source_root}/"}"
		target_agent_file="${agents_target_root}/${relative_agent_path}"
		target_parent_dir="$(dirname "${target_agent_file}")"

		mkdir -p "${target_parent_dir}"

		if [ -L "${target_agent_file}" ]; then
			current_link_target="$(readlink "${target_agent_file}")"
			if [ "${current_link_target}" = "${agent_file}" ]; then
				log_message INFO "Agent already linked: \`${relative_agent_path}\`"
				continue
			fi
			rm -f "${target_agent_file}"
		elif [ -e "${target_agent_file}" ]; then
			log_message WARN "Agent target exists and is not a symlink: \`${target_agent_file}\`. Skipping."
			continue
		fi

		ln -s "${agent_file}" "${target_agent_file}"
		log_message SUCCESS "Linked Claude agent: \`${relative_agent_path}\`"
	done < <(find "${agents_source_root}" -type f -name '*.md' -print0)

	if [ "${agents_count}" -eq 0 ]; then
		log_message WARN "No agent markdown files found under \`${agents_source_root}\`. Nothing to link."
	else
		log_message SUCCESS "Linked ${agents_count} Claude agent(s) into \`${agents_target_root}\`."
	fi
else
	log_message WARN "No packaged Claude agents found at \`${agents_source_root}\`. Skipping agent symlinks."
fi

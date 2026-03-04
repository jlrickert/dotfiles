#!/usr/bin/env bash
# Claude Code status line - inspired by ~/.bashrc __ps1 short form prompt

input=$(cat)

user=$(whoami)
host=$(hostname -s)
raw_dir=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
# If inside a git repo, show just the basename of the git root
git_root=$(git -C "$raw_dir" --no-optional-locks rev-parse --show-toplevel 2>/dev/null)
if [ -n "$git_root" ]; then
    dir=$(basename "$git_root")
else
    # Shorten home directory to ~
    dir="${raw_dir/#$HOME/\~}"
fi

model=$(echo "$input" | jq -r '.model.display_name // ""')

session_name=$(echo "$input" | jq -r '.session_name // empty')
if [ -n "$session_name" ]; then
    session_display=" \"${session_name}\""
else
    session_display=""
fi

used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
if [ -n "$used" ]; then
    used_display=" ctx:${used}%"
else
    used_display=""
fi

effort=$(jq -r '.effortLevel // empty' ~/.claude/settings.json 2>/dev/null)
if [ -n "$effort" ]; then
    effort_display=" effort:${effort}"
else
    effort_display=""
fi

# Git branch (skip optional locks)
branch=$(git -C "$raw_dir" --no-optional-locks branch --show-current 2>/dev/null)
if [ -n "$branch" ]; then
    branch_display="($branch)"
else
    branch_display=""
fi

# Colors (these will appear dimmed in the status line)
r='\e[31m'
g='\e[32m'
h='\e[34m'
u='\e[33m'
w='\e[35m'
b='\e[36m'
x='\e[0m'

printf "${u}%s${g}@${h}%s${g}:${w}%s${b}%s${g} [%s]${x}%s%s%s" \
    "$user" "$host" "$dir" "$branch_display" "$model" "$used_display" "$effort_display" "$session_display"

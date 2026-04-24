#!/usr/bin/env bash
# Shared assertion helpers for end-to-end shell tests.

PASS_COUNT=0
FAIL_COUNT=0

pass() {
    PASS_COUNT=$((PASS_COUNT + 1))
    printf 'PASS: %s\n' "$1"
}

fail() {
    FAIL_COUNT=$((FAIL_COUNT + 1))
    printf 'FAIL: %s\n' "$1" >&2
}

assert_link() {
    local path="$1"
    if [ ! -L "$path" ]; then
        fail "$path is not a symlink"
        return
    fi
    if [ ! -e "$path" ]; then
        fail "$path -> $(readlink "$path") (target missing)"
        return
    fi
    pass "$path -> $(readlink "$path")"
}

assert_file() {
    local path="$1"
    if [ ! -f "$path" ]; then
        fail "$path is not a regular file"
        return
    fi
    pass "$path is a regular file"
}

assert_cmd() {
    local cmd="$1"
    if command -v "$cmd" >/dev/null 2>&1; then
        pass "$cmd available at $(command -v "$cmd")"
    else
        fail "$cmd not on PATH"
    fi
}

assert_shell_loads() {
    local shell="$1"
    if "$shell" -i -c 'echo READY' 2>/dev/null | grep -qx READY; then
        pass "$shell -i -c works"
    else
        fail "$shell -i -c failed (rc files have errors)"
    fi
}

summary() {
    printf '\n%d passed, %d failed\n' "$PASS_COUNT" "$FAIL_COUNT"
    [ "$FAIL_COUNT" -eq 0 ]
}

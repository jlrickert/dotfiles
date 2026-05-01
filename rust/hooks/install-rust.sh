#!/usr/bin/env bash
set -o errexit
set -o nounset
set -o pipefail

# Bail if cargo is already on PATH (system rust, asdf, manual install, …).
# We never want to clobber another rust install with rustup.
if command -v cargo >/dev/null 2>&1; then
	echo "rust: cargo already installed at $(command -v cargo); skipping rustup."
	exit 0
fi

# Pin CARGO_HOME / RUSTUP_HOME under XDG_DATA_HOME, mirroring profile.d/rust.sh.
# rustup respects these env vars during install; without them it would default
# to ~/.cargo and ~/.rustup, which we deliberately avoid.
: "${XDG_DATA_HOME:=$HOME/.local/share}"
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"

# Stage the rustup-init script in a tempdir we own so curl failures and
# the trailing install can be cleaned up reliably.
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

echo "rust: downloading rustup-init from https://sh.rustup.rs"
curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs -o "${tmp}/rustup-init.sh"

# -y: non-interactive defaults. --no-modify-path: don't append to ~/.profile;
# we manage PATH via dots-config/profile.d so the install stays idempotent.
echo "rust: running rustup-init -y --no-modify-path"
bash "${tmp}/rustup-init.sh" -y --no-modify-path

echo "rust: rustup installed; cargo at ${CARGO_HOME}/bin/cargo"

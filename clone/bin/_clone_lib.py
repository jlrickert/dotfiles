"""Shared post-clone gate for gh-clone / bb-clone.

Stdlib-only (Python 3.11+). Public API:
    parse_github(spec) -> (owner, name, url)
    parse_bitbucket(spec) -> (workspace, name, url)
    parse_file_url(url) -> (owner, name)        # owner is always "local"
    repos_root() -> Path
    run_post_clone_gate(cloned_path, owner, name, repos_root) -> int
"""

from __future__ import annotations

import json
import os
import re
import shutil
import sys
import tomllib
from datetime import datetime, timezone
from pathlib import Path


# --- URL / shorthand parsing -------------------------------------------------

# Strip an optional trailing ".git". Both forge SSH and HTTPS clone URLs accept
# the suffix; we normalize it off so the on-disk path doesn't end in ".git".
_DOTGIT = re.compile(r"\.git$")


def _strip_git(s):
    return _DOTGIT.sub("", s)


def parse_github(spec):
    """Return (owner, name, clone_url) from a GitHub spec.

    Accepts:
        owner/name              -> https://github.com/owner/name.git
        https://github.com/owner/name(.git)?
        git@github.com:owner/name(.git)?
        bare-name               -> ($GITUSER || $USER) / bare-name
        file://...              -> handled by parse_file_url; caller must
                                   dispatch on scheme before calling here.
    """
    s = spec.strip()
    # SSH: git@github.com:owner/name
    m = re.match(r"^git@github\.com:([^/]+)/(.+?)(?:\.git)?$", s)
    if m:
        owner, name = m.group(1), m.group(2)
        return owner, name, f"git@github.com:{owner}/{name}.git"
    # HTTPS: https://github.com/owner/name
    m = re.match(r"^https://github\.com/([^/]+)/(.+?)(?:\.git)?/?$", s)
    if m:
        owner, name = m.group(1), m.group(2)
        return owner, name, f"https://github.com/{owner}/{name}.git"
    # owner/name shorthand (no scheme, exactly one slash, no whitespace).
    if "/" in s and "://" not in s and " " not in s:
        owner, _, name = s.partition("/")
        name = _strip_git(name)
        if owner and name:
            return owner, name, f"https://github.com/{owner}/{name}.git"
    # Bare name -> $GITUSER, fall back to $USER.
    if s and "/" not in s and "://" not in s:
        owner = os.environ.get("GITUSER") or os.environ.get("USER") or ""
        if not owner:
            raise ValueError(
                f"bare repo name '{s}' requires $GITUSER or $USER to be set"
            )
        name = _strip_git(s)
        return owner, name, f"https://github.com/{owner}/{name}.git"
    raise ValueError(f"unrecognized github spec: {spec!r}")


def parse_bitbucket(spec):
    """Return (workspace, name, clone_url) from a Bitbucket spec.

    Accepts:
        workspace/name          -> git@bitbucket.org:workspace/name.git
        git@bitbucket.org:workspace/name(.git)?
        https://bitbucket.org/workspace/name(.git)?
        bare-name               -> requires $BB_WORKSPACE (no $BBUSER fallback).
    """
    s = spec.strip()
    m = re.match(r"^git@bitbucket\.org:([^/]+)/(.+?)(?:\.git)?$", s)
    if m:
        ws, name = m.group(1), m.group(2)
        return ws, name, f"git@bitbucket.org:{ws}/{name}.git"
    m = re.match(r"^https://bitbucket\.org/([^/]+)/(.+?)(?:\.git)?/?$", s)
    if m:
        ws, name = m.group(1), m.group(2)
        return ws, name, f"https://bitbucket.org/{ws}/{name}.git"
    if "/" in s and "://" not in s and " " not in s:
        ws, _, name = s.partition("/")
        name = _strip_git(name)
        if ws and name:
            # Default to SSH for shorthand (matches the legacy bb workflow).
            return ws, name, f"git@bitbucket.org:{ws}/{name}.git"
    if s and "/" not in s and "://" not in s:
        ws = os.environ.get("BB_WORKSPACE", "")
        if not ws:
            raise ValueError(
                f"bare repo name '{s}' requires $BB_WORKSPACE to be set"
            )
        name = _strip_git(s)
        return ws, name, f"git@bitbucket.org:{ws}/{name}.git"
    raise ValueError(f"unrecognized bitbucket spec: {spec!r}")


def parse_file_url(url):
    """Return (owner='local', name) for a file:// URL. Used by fixture tests."""
    if not url.startswith("file://"):
        raise ValueError(f"not a file:// url: {url!r}")
    path = url[len("file://"):]
    name = _strip_git(os.path.basename(path.rstrip("/")))
    return "local", name


def repos_root():
    """$REPOS or ~/repos."""
    r = os.environ.get("REPOS")
    if r:
        return Path(r)
    return Path.home() / "repos"


# --- Detector registry -------------------------------------------------------

# Each detector returns a list of finding strings (empty == no hit).
# We pass the worktree root as a Path. Predicates are pure file-system reads;
# no execution, no network.

def _file_exists(rel):
    def predicate(root):
        return [rel] if (root / rel).is_file() else []
    return predicate


def _dir_nonempty(rel):
    def predicate(root):
        p = root / rel
        if p.is_dir():
            try:
                if any(p.iterdir()):
                    return [rel + "/"]
            except OSError:
                pass
        return []
    return predicate


def _glob_match(pattern):
    def predicate(root):
        hits = []
        for m in root.glob(pattern):
            if m.is_file():
                hits.append(str(m.relative_to(root)))
        return hits
    return predicate


def _package_json_lifecycle(root):
    """Block if package.json declares preinstall/postinstall/prepare scripts."""
    pj = root / "package.json"
    if not pj.is_file():
        return []
    try:
        data = json.loads(pj.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return []
    scripts = data.get("scripts") or {}
    hits = []
    for key in ("preinstall", "postinstall", "prepare"):
        if isinstance(scripts, dict) and key in scripts:
            hits.append(f"package.json:scripts.{key}")
    return hits


def _pyproject_build_hooks(root):
    """Block on build-system table; tomllib parse error is also a block."""
    pp = root / "pyproject.toml"
    if not pp.is_file():
        return []
    try:
        with pp.open("rb") as fh:
            data = tomllib.load(fh)
    except tomllib.TOMLDecodeError:
        return ["pyproject.toml (unparseable — manual review required)"]
    except OSError:
        return []
    if "build-system" in data:
        return ["pyproject.toml:build-system"]
    return []


def _git_hooks_in_worktree(root):
    """Block on non-.sample hook files committed to .git/hooks/.

    A normal `git clone` populates .git/hooks/ with .sample stubs only;
    real hooks shipped via the worktree are an unusual sign of intent.
    """
    hooks = root / ".git" / "hooks"
    if not hooks.is_dir():
        return []
    hits = []
    try:
        for entry in hooks.iterdir():
            if entry.is_file() and not entry.name.endswith(".sample"):
                hits.append(f".git/hooks/{entry.name}")
    except OSError:
        pass
    return hits


def _core_hookspath_override(root):
    """Block on `core.hooksPath = ...` in .git/config (supply-chain dodge)."""
    cfg = root / ".git" / "config"
    if not cfg.is_file():
        return []
    try:
        text = cfg.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return []
    # Match the bare key under any [core] section. Conservative: any
    # occurrence of `hooksPath = ...` triggers; false positives are cheap
    # because the fix is "review the cloned tree manually".
    if re.search(r"(?mi)^\s*hooksPath\s*=", text):
        return [".git/config:core.hooksPath"]
    return []


# (name, predicate, severity, description)
DETECTORS = [
    # --- block-set ---
    ("claude-settings", _file_exists(".claude/settings.json"), "block",
     "Claude Code settings file"),
    ("claude-settings-local", _file_exists(".claude/settings.local.json"),
     "block", "Claude Code local settings file"),
    ("vscode-tasks", _file_exists(".vscode/tasks.json"), "block",
     "VS Code tasks (auto-run on open in some configs)"),
    ("vscode-launch", _file_exists(".vscode/launch.json"), "block",
     "VS Code launch configuration"),
    ("vscode-settings", _file_exists(".vscode/settings.json"), "block",
     "VS Code workspace settings"),
    ("envrc", _file_exists(".envrc"), "block",
     "direnv .envrc (auto-eval on cd)"),
    ("devcontainer", _file_exists(".devcontainer/devcontainer.json"), "block",
     "devcontainer manifest (auto-build on open)"),
    ("pre-commit", _file_exists(".pre-commit-config.yaml"), "block",
     "pre-commit hook config"),
    ("husky", _dir_nonempty(".husky"), "block",
     "husky hook directory"),
    ("package-json-lifecycle", _package_json_lifecycle, "block",
     "package.json lifecycle scripts (preinstall/postinstall/prepare)"),
    ("setup-py", _file_exists("setup.py"), "block",
     "setup.py (executes on pip install)"),
    ("pyproject-build", _pyproject_build_hooks, "block",
     "pyproject.toml build-system hooks"),
    ("git-hooks", _git_hooks_in_worktree, "block",
     "non-sample files in .git/hooks/"),
    ("hookspath", _core_hookspath_override, "block",
     "core.hooksPath override"),
    # --- advisory-set ---
    ("makefile", _file_exists("Makefile"), "advisory", "Makefile present"),
    ("justfile", _file_exists("justfile"), "advisory", "justfile present"),
    ("taskfile", _file_exists("Taskfile.yml"), "advisory",
     "Taskfile.yml present"),
    ("idea-runconfig", _dir_nonempty(".idea/runConfigurations"), "advisory",
     "JetBrains run configurations"),
    ("gh-actions", _glob_match(".github/workflows/*"), "advisory",
     "GitHub Actions workflows"),
    ("bb-pipelines", _file_exists("bitbucket-pipelines.yml"), "advisory",
     "Bitbucket Pipelines config"),
]


def scan(root):
    """Walk the tree once via the detector registry. Returns (blocks, advisories).

    Each list entry is a string of the form:
        "<detector-name>: <description> (<finding>)"
    """
    blocks, advisories = [], []
    for name, predicate, severity, desc in DETECTORS:
        try:
            hits = predicate(root)
        except Exception as exc:  # pragma: no cover - defensive
            hits = [f"<detector error: {exc}>"]
        for h in hits:
            line = f"{name}: {desc} ({h})"
            (blocks if severity == "block" else advisories).append(line)
    return blocks, advisories


# --- Quarantine + disposition -----------------------------------------------

def quarantine(cloned_path, owner, name, repos_root_path):
    """Move the cloned tree under <repos_root>/.untrusted/<owner>/<name>/.

    On collision, append a UTC timestamp suffix.
    """
    dest_parent = repos_root_path / ".untrusted" / owner
    dest_parent.mkdir(parents=True, exist_ok=True)
    dest = dest_parent / name
    if dest.exists():
        stamp = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
        dest = dest_parent / f"{name}.{stamp}"
    shutil.move(str(cloned_path), str(dest))
    return dest


def run_post_clone_gate(cloned_path, owner, name, repos_root_path):
    """Run the gate. Returns the process exit code (0 ok, non-zero on block)."""
    cloned_path = Path(cloned_path)
    repos_root_path = Path(repos_root_path)
    blocks, advisories = scan(cloned_path)

    if blocks:
        dest = quarantine(cloned_path, owner, name, repos_root_path)
        print(
            f"\nclone gate: BLOCKED — {len(blocks)} finding(s); "
            f"moved to {dest}",
            file=sys.stderr,
        )
        for i, line in enumerate(blocks, 1):
            print(f"  {i}. {line}", file=sys.stderr)
        if advisories:
            print(
                f"clone gate: also {len(advisories)} advisory finding(s):",
                file=sys.stderr,
            )
            for i, line in enumerate(advisories, 1):
                print(f"  {i}. {line}", file=sys.stderr)
        return 2

    if advisories:
        print(
            f"clone gate: {len(advisories)} advisory finding(s) at "
            f"{cloned_path}:",
            file=sys.stderr,
        )
        for i, line in enumerate(advisories, 1):
            print(f"  {i}. {line}", file=sys.stderr)
    return 0

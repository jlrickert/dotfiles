---
name: tap-keg
description: "Query, navigate, and maintain notes in a KEG using the tap CLI. Use when a user asks for KEG workflows: list kegs or nodes, inspect notes by NODEID, find backlinks, search note text, filter by tag expression, create or edit notes, or apply entity rules from the keg config."
---

# Tap KEG

Use `tap` as the source of truth for KEG retrieval and navigation. All file access and editing must go through `tap`. Do not directly read or write node files.

## KEG Selection

A default KEG is always selected. Use `-k KEG` only when targeting a non-default keg. All commands below omit `-k` for brevity; add `-k KEG` when needed.

## Bootstrapping

On startup, orient with `tap info` to determine the default keg, working directory, and target path. Use `tap repo list` to discover all available KEG aliases. Read system notes for each relevant keg with `tap config`.

## Project Tagging Convention

Notes related to a project are tagged with the project directory name (last path component of the git working directory). For example, `/Users/jlrickert/repos/github.com/jlrickert/tapper` uses tag `tapper`.

**Exception for `ecw` keg**: use the project name prefix, not the full directory. For example, `/Users/jlrickert/repos/bitbucket.org/ecw-devel/rohne.202602` uses tag `rohne`.

## Workflow

1. Identify the target KEG alias from user input.
2. When KEG alias is missing, discover candidates:

```bash
tap repo list
```

3. If the request involves entity types or node structure requirements, read KEG config:

```bash
tap config
```

4. Use `entities:` in `keg` as canonical mapping: `entity-type -> id` (entity note).
5. Load the mapped canonical entity note when creating or updating that entity type:

```bash
tap cat NODEID
```

6. Start broad discovery when context is missing:

```bash
tap list
tap tags
```

7. Narrow by tag or expression when provided:

```bash
tap tags TAG_OR_EXPR
```

8. Use `--query` to filter with boolean expressions over tags and attributes. See `tap docs query-expressions` for full syntax.

```bash
tap list --query "golang and entity=concept"
tap list --query "not system"
```

9. Inspect note details or references:

```bash
tap cat NODEID
tap backlinks NODEID
tap grep QUERY
```

10. Create or update via stdin (no editor launched when stdin is piped):

```bash
CONTENT | tap create
CONTENT | tap edit NODEID
CONTENT | tap meta NODEID
```

11. Rebuild indexes when indexing is part of the task:

```bash
tap reindex
```

## Entity Types

Use `tap config` to read the `entities:` section as canonical for structure requirements.

- Regular notes: `meta.yaml` `entity` is the note type.
- Entity notes: notes that define structure requirements for an entity type.

When asked to create or update notes of a type:

1. Read `tap config` and find `entities.<type>.id`.
2. Open that entity note with `tap cat <id>`.
3. Apply those requirements to the target note.

If `entities.<type>` is missing, report it and propose creating a canonical entity note plus `keg` mapping update.

## Create Or Update Nodes

When creating or revising KEG nodes, load `references/keg-system-guide.md` and enforce its rules.

Use these `tap` commands:

```bash
CONTENT | tap create
tap edit NODEID
tap meta NODEID --edit
tap config edit
tap reindex
```

Commands that modify content (`create`, `edit`, `meta`, `config edit`) accept stdin. When stdin is piped, the content is applied directly without opening an editor:

```bash
CONTENT | tap create
CONTENT | tap edit NODEID
CONTENT | tap meta NODEID
```

## Linking Conventions

- Internal KEG link format: `../NODEID`.
- Cross-KEG link format: `keg:KEG/NODEID`.
- `KEG` may be a KEG name or alias from the local `keg` file `links` section.

## System Notes

System notes are internal KEG infrastructure nodes (indexes, entity definitions, config-related nodes). They must be tagged with `system`. Use `tap list --query system` to find them. Do not create system notes unless the user explicitly requests it.

## Command Patterns

Use these exact patterns (add `-k KEG` when targeting a non-default keg):

```bash
tap repo list
tap info
tap list
tap list --query EXPR
tap tags
tap tags TAG_OR_EXPR
tap cat NODEID
tap backlinks NODEID
tap grep QUERY
CONTENT | tap create
tap edit NODEID
CONTENT | tap edit NODEID
tap meta NODEID --edit
CONTENT | tap meta NODEID
tap config
tap config edit
tap reindex
```

Replace placeholders:

- `TAG_OR_EXPR`: tag value or tag expression
- `NODEID`: note identifier
- `QUERY`: ripgrep-style query
- `EXPR`: boolean query expression (see `tap docs query-expressions`)

## Built-in Documentation

Use `tap docs` to list available topics. Use `tap docs TOPIC` to read a topic. Use `tap COMMAND --help` for command-specific usage.

Key topics:

- `tap docs query-expressions` — query syntax for `--query` flag
- `tap docs keg-structure/minimum-node` — minimum node requirements
- `tap docs keg-structure/entity-and-tag-patterns` — entity and tag conventions
- `tap docs keg-structure/markdown-style-guide` — markdown formatting rules
- `tap docs configuration/keg-config` — keg configuration format

## References

- Load `references/keg-system-guide.md` for KEG layout, node templates, metadata rules, linking conventions, and tagging strategy.

## Response Rules

- Return command output as concise, task-focused summaries.
- Quote exact note text only when asked for verbatim output.
- If lookup returns no result, report it directly and suggest the nearest discovery command.
- Prefer incremental retrieval (`list` and `tags`) before broad assumptions about note structure.

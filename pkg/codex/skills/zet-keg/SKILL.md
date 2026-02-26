---
name: zet-keg
description: Query, navigate, and maintain notes in a zet knowledge exchange graph (KEG) from the CLI. Use when a user asks to list notes, inspect a note by NODEID, find backlinks, search note text, filter notes by tag, create or validate KEG nodes, or work with KEG entity types using canonical entity-note mappings from the KEG config file.
---

# Zet KEG

Use `zet` as the source of truth for KEG retrieval and navigation.

## Workflow

1. Identify the target KEG name from user input.
2. Resolve KEG and node working directories when path context is needed:
  ```bash
  zet KEG pwd
  zet KEG pwd NODEID
  ```
3. If the request involves entity types or node structure requirements, read KEG config:

  ```bash
  cat "$(zet KEG pwd)/keg"
  ```
4. Use `entities:` in `keg` as canonical mapping: `entity-type -> id` (entity note).
5. Load the mapped canonical entity note when creating/updating that entity type:

  ```bash
  zet KEG cat NODEID
  ```
6. Create a new note from stdin when the user provides note content directly:

  ```bash
  zet KEG c -
  ```
7. Start broad discovery when context is missing:

  ```bash
  zet KEG list
  zet KEG tags
  ```
8. Narrow by tag when provided:

  ```bash
  zet KEG tags TAG
  ```
9. Inspect note details or references:

  ```bash
  zet KEG cat NODEID
  zet KEG backlinks NODEID
  ```
10. Search KEG text when needed:
  ```bash
  zet KEG grep QUERY
  ```

## Entity Types

Treat `$(zet KEG pwd)/keg` `entities:` as canonical for structure requirements.

- **Regular notes**: `meta.yaml` `entity` is the note type (e.g., `task`, `patch`, `concept`, `guide`).
- **Entity notes**: notes that define structure requirements for an entity type.
  - H1 should start with `Entity:` and include slug in parentheses.
  - `meta.yaml` should use `entity: entity`.
  - Tags should live in `stats.yaml`; mirror tags into `meta.yaml` for compatibility.

When asked to create/update notes of a type:
1. Read `keg` and find `entities.<type>.id`.
2. Open that entity note with `zet KEG cat <id>`.
3. Apply those requirements to the target note.

If `entities.<type>` is missing, report it and propose creating a canonical entity note plus `keg` mapping update.

## Create Or Update Nodes

When creating or revising KEG nodes, load `references/keg-system-guide.md` and enforce its rules:

1. Create or edit `docs/NODEID/README.md` with one H1 first line (e.g., `# Task: Database migration - Customer Platform`) and a 1-3 sentence lead paragraph.
2. Create or edit `docs/NODEID/stats.yaml` with required fields (`created`, `updated`, `title`, `entity`, `tags`).
3. Create or edit `docs/NODEID/meta.yaml` with compatibility fields (`created`, `updated`, `title`, `entity`, `tags`) mirrored from `stats.yaml`.
4. Keep `meta.yaml` and `stats.yaml` `title` exactly aligned with README H1.
5. Keep tags lowercase and hyphen-separated, and keep tags synchronized in both `stats.yaml` and `meta.yaml`.
6. Use internal links in `README.md` with `../NODEID` format and cross-KEG links with `keg:KEG/NODEID` (example: `keg:pub/123` links to note `123` in KEG `pub`).
7. For stdin-driven note creation, pass full note content to:

  ```bash
  zet KEG c -
  ```
8. If indexing is part of the task, run:

  ```bash
  zet KEG index
  ```

## Linking Conventions

- Internal KEG link format: `../NODEID`.
- Cross-KEG link format: `keg:KEG/NODEID`.
- `KEG` may be a KEG name or alias from the local `keg` file `links` section.
- Example: `keg:pub/123` means note `123` in KEG `pub`.

## Command Patterns

Use these exact patterns:

```bash
zet KEG pwd
zet KEG pwd NODEID
zet KEG c -
zet KEG list
zet KEG tags
zet KEG tags TAG
zet KEG cat NODEID
zet KEG backlinks NODEID
zet KEG grep QUERY
zet KEG index
```

Also use this config lookup when entity requirements are needed:

```bash
cat "$(zet KEG pwd)/keg"
```

Replace placeholders:
- `KEG`: knowledge exchange graph name
- `TAG`: tag value such as `person`
- `NODEID`: note identifier
- `QUERY`: ripgrep-style search query

Notes:
- `zet KEG pwd` returns the KEG working directory path.
- `zet KEG pwd NODEID` returns the working directory path for that node.
- `zet KEG c -` creates a note from stdin.

## References

- Load `references/keg-system-guide.md` for KEG layout, node templates, metadata rules, linking conventions, tagging strategy, and node type conventions.

## Response Rules

- Return command output as concise, task-focused summaries.
- Quote exact note text only when the user asks for verbatim output.
- If a lookup returns no result, report it directly and suggest the nearest valid discovery command.
- Prefer incremental retrieval (`list`/`tags`) before broad assumptions about note structure.

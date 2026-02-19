---
name: zet-keg
description: Query, navigate, and maintain notes in a zet knowledge exchange graph (KEG) from the CLI. Use when a user asks to list notes, inspect a note by NODEID, find backlinks, search note text, filter notes by tag, create or validate KEG node structure (README.md and meta.yaml), apply KEG tagging/title conventions, or discover available KEG subcommands via zet.
---

# Zet KEG

Use `zet` as the source of truth for note retrieval and navigation.

## Workflow

1. Identify the target KEG name from user input.
2. Resolve KEG and node working directories when path context is needed:
```bash
zet KEG pwd
zet KEG pwd NODEID
```
3. Start with broad discovery when context is missing:
```bash
zet KEG list
zet KEG tags
```
4. Narrow to specific note sets when the user provides a tag:
```bash
zet KEG tags TAG
```
5. Inspect a specific note when the user provides a node id:
```bash
zet KEG cat NODEID
```
6. Find incoming links to a note when the user asks what references it:
```bash
zet KEG backlinks NODEID
```
7. Run text search across the KEG when the user asks for keyword or phrase matches:
```bash
zet KEG grep QUERY
```

## Create Or Update Nodes

When the user asks to create or revise KEG nodes, load `references/keg-system-guide.md` and enforce its rules:

1. Create or edit `docs/NODEID/README.md` with a single H1 first line and a 1-3 sentence lead paragraph.
2. Create or edit `docs/NODEID/meta.yaml` with required fields (`created`, `updated`, `title`, `entity`, `tags`).
3. Keep `meta.yaml` `title` exactly aligned with README H1.
4. Keep tags lowercase and hyphen-separated.
5. Use internal links in `README.md` with `../NODEID` format.
6. If indexing is part of the task, run:
```bash
zet index
```

## Command Patterns

Use these exact patterns:

```bash
zet KEG pwd
zet KEG pwd NODEID
zet KEG list
zet KEG tags
zet KEG tags TAG
zet KEG cat NODEID
zet KEG backlinks NODEID
zet KEG grep QUERY
zet index
```

Replace placeholders:
- `KEG`: knowledge exchange graph name
- `TAG`: tag value such as `person`
- `NODEID`: note identifier
- `QUERY`: ripgrep-style search query

Notes:
- `zet KEG pwd` returns the KEG working directory path.
- `zet KEG pwd NODEID` returns the working directory path for that node.

## References

- Load `references/keg-system-guide.md` for KEG file layout, node templates, metadata rules, linking conventions, and tagging strategy.

## Response Rules

- Return command output as concise, task-focused summaries.
- Quote exact note text only when the user asks for verbatim output.
- If a lookup returns no result, report it directly and suggest the nearest valid discovery command.
- Prefer incremental retrieval (`list`/`tags`) before broad assumptions about note structure.

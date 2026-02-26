---
name: zet-search
description: Search, discover, and read notes in a zet KEG. Use when looking up notes by tag or keyword, finding backlinks, listing notes, or reading a specific note.
---

# Zet Search

Use `zet` as the source of truth for KEG discovery and retrieval.

## Workflow

1. Identify the target KEG name from user input.
2. Resolve KEG working directory when path context is needed:

```bash
zet KEG pwd
zet KEG pwd NODEID
```

3. Start broad discovery when context is missing:

```bash
zet KEG list
zet KEG tags
```

4. Narrow by tag when provided:

```bash
zet KEG tags TAG
```

5. Inspect note details or references:

```bash
zet KEG cat NODEID
zet KEG backlinks NODEID
```

6. Search KEG text when needed:

```bash
zet KEG grep QUERY
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
```

Replace placeholders:

- `KEG`: knowledge exchange graph name
- `TAG`: tag value such as `person`
- `NODEID`: note identifier
- `QUERY`: ripgrep-style search query

Notes:

- `zet KEG pwd` returns the KEG working directory path.
- `zet KEG pwd NODEID` returns the working directory path for that node.

## Response Rules

- Return command output as concise, task-focused summaries.
- Quote exact note text only when the user asks for verbatim output.
- If a lookup returns no result, report it directly and suggest the nearest valid discovery command.
- Prefer incremental retrieval (`list`/`tags`) before broad assumptions about note structure.

## References

- Load [references/keg-system-guide.md](references/keg-system-guide.md) for KEG layout, node templates, metadata rules, linking conventions, tagging strategy, and node type conventions.

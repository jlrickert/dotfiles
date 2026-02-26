---
name: zet-create
description: "Create a new note in a zet KEG. Use when adding a new knowledge node, documenting a finding, or recording information that should be preserved."
disable-model-invocation: false
---

# Zet Create

Use `zet` to create new nodes in a KEG. Load `references/keg-system-guide.md` and enforce its rules when creating nodes.

## Workflow

1. Identify the target KEG name from user input.
2. Resolve KEG working directory:

```bash
zet KEG pwd
```

3. If the request involves entity types, read KEG config:

```bash
cat "$(zet KEG pwd)/keg"
```

4. Use `entities:` in `keg` as canonical mapping: `entity-type -> id` (entity note).
5. Load the mapped canonical entity note when creating that entity type:

```bash
zet KEG cat NODEID
```

6. Apply entity requirements to the new node.
7. Create a new note from stdin when the user provides note content directly:

```bash
zet KEG c -
```

8. After creation, run index if indexing is part of the task:

```bash
zet KEG index
```

## Node Structure

When creating nodes, enforce these rules:

1. Create `docs/NODEID/README.md` with one H1 first line and a 1-3 sentence lead paragraph.
2. Create `docs/NODEID/stats.yaml` with required fields: `created`, `updated`, `title`, `entity`, `tags`.
3. Create `docs/NODEID/meta.yaml` with compatibility fields mirroring `stats.yaml`: `created`, `updated`, `title`, `entity`, `tags`.
4. Keep `meta.yaml` and `stats.yaml` `title` exactly aligned with README H1.
5. Keep tags lowercase and hyphen-separated, synchronized in both `stats.yaml` and `meta.yaml`.
6. Use internal links in `README.md` with `../NODEID` format and cross-KEG links with `keg:KEG/NODEID`.

## Command Patterns

```bash
zet KEG pwd
zet KEG c -
zet KEG cat NODEID
zet KEG index
cat "$(zet KEG pwd)/keg"
```

Replace placeholders:

- `KEG`: knowledge exchange graph name
- `NODEID`: note identifier

Notes:

- `zet KEG c -` creates a note from stdin.

## References

- Load [references/keg-system-guide.md](references/keg-system-guide.md) for KEG layout, node templates, metadata rules, linking conventions, tagging strategy, and node type conventions.

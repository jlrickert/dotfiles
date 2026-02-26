---
name: zet-update
description: Update an existing note in a zet KEG. Use when revising, correcting, or extending a knowledge node, or when synchronizing metadata.
disable-model-invocation: false
---

# Zet Update

Use `zet` to update existing nodes in a KEG. Load `references/keg-system-guide.md` and enforce its rules when editing nodes.

## Workflow

1. Identify the target KEG name and NODEID from user input.
2. Resolve the node working directory:

```bash
zet KEG pwd NODEID
```

3. Read the current node content:

```bash
zet KEG cat NODEID
```

4. If the request involves entity types, read KEG config:

```bash
cat "$(zet KEG pwd)/keg"
```

5. Use `entities:` in `keg` as canonical mapping: `entity-type -> id` (entity note).
6. Load the mapped canonical entity note for structure requirements:

```bash
zet KEG cat NODEID
```

7. Edit `README.md`, keeping H1 aligned with `stats.yaml` and `meta.yaml` title.
8. Sync tags between `stats.yaml` and `meta.yaml`.
9. Run index if indexing is part of the task:

```bash
zet KEG index
```

## Update Rules

When editing existing nodes, enforce these rules:

1. Edit `docs/NODEID/README.md` — keep H1 as the first line; update lead paragraph if needed.
2. Edit `docs/NODEID/stats.yaml` — update `updated` timestamp; keep `created` unchanged; keep `title` exactly aligned with README H1.
3. Edit `docs/NODEID/meta.yaml` — mirror `created`, `updated`, `title`, `entity`, and `tags` from `stats.yaml`.
4. Keep tags lowercase and hyphen-separated, synchronized in both `stats.yaml` and `meta.yaml`.
5. Use internal links in `README.md` with `../NODEID` format and cross-KEG links with `keg:KEG/NODEID`.

## Command Patterns

```bash
zet KEG pwd NODEID
zet KEG cat NODEID
zet KEG index
cat "$(zet KEG pwd)/keg"
```

Replace placeholders:

- `KEG`: knowledge exchange graph name
- `NODEID`: note identifier

## References

- Load [references/keg-system-guide.md](references/keg-system-guide.md) for KEG layout, node templates, metadata rules, linking conventions, tagging strategy, and node type conventions.

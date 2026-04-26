# KEG System Guide

## Overview

A KEG (Knowledge Exchange Graph) is a zettelkasten-style note repository in Git. Each node is a numbered directory with markdown content and YAML metadata.

Core characteristics:

- Version controlled in Git
- Numbered node directories: `docs/1/`, `docs/2/`, ...
- Node files: `README.md`, `stats.yaml`, and `meta.yaml`
- `stats.yaml` stores canonical metadata fields (`created`, `updated`, `title`, `entity`, `tags`)
- `meta.yaml` mirrors compatibility fields and may include optional user-managed metadata
- Stats tooling is not yet available; maintain `stats.yaml` manually for now
- Auto-generated indices under `docs/dex/`
- Discovery via tags, backlinks, and text search

## Directory Layout

```text
docs/
├── 1/
│   ├── README.md
│   ├── stats.yaml
│   └── meta.yaml
├── 2/
│   ├── README.md
│   ├── stats.yaml
│   └── meta.yaml
└── dex/
    ├── nodes.tsv
    ├── tags
    └── ...
```

## README.md Rules

### H1 First Line (required)

Use one H1 first line in this shape:

```text
# [Node Type]: [Content] - [Context] [(optional-slug)]
```

Examples:

- `# Task: Database migration - Customer Platform`
- `# PR: Authentication refactor - Auth Service`
- `# Reference: REST API Best Practices (rest-api-best-practices)`
- `# Project: Customer Portal (customer-portal-v2)`

### Lead Paragraph (required)

Add a 1-3 sentence summary directly under the H1.

### Internal Linking

Use relative links for KEG-local references:

```markdown
[Link Text](../123)
```

Rules:

- Use `../NODEID` for internal links
- Avoid absolute URLs for internal KEG references
- Prefer descriptive link text

### External KEG Linking

Use `keg:KEG/NODEID` to reference notes in another KEG:

- `keg:project-notes/42`
- `keg:keg-system-guide/108`
- `keg:pub/123`

### Typical H2 Sections

Use applicable sections:

- `## Overview` / `## Summary` / `## Purpose`
- `## Details` / `## Requirements` / `## Changes`
- `## Subtasks` / `## Steps` / `## Actions`
- `## Time log` / `## Progress`
- `## Deployment` / `## Go live` / `## Testing`
- `## Patch` / `## PR` / `## Related Work`
- `## See also` / `## References`

## stats.yaml Rules

`stats.yaml` is the source of truth for `created`, `updated`, `title`, `entity`, and `tags`. Tooling is expected to manage this file, but update it manually until those tools are available.

Required fields:

```yaml
created: 2024-01-15T10:30:00Z
updated: 2024-01-15T10:30:00Z
title: "Task: Database migration - Customer Platform"
entity: task
tags:
  - task
  - database
  - migration
```

Field rules:

- `created`: set once at creation; never change
- `updated`: set to current UTC ISO 8601 timestamp for meaningful edits
- `title`: must match README H1 exactly (including checkbox status if present)
- `entity`: lowercase category
- `tags`: lowercase, hyphen-separated discovery tokens

## meta.yaml Rules (Compatibility + User-Managed Data)

Use `meta.yaml` as a compatibility mirror for key fields from `stats.yaml` plus optional user-managed metadata fields.

Keep fields `created`, `updated`, `title`, `entity`, and `tags` present for compatibility, and mirror those values from `stats.yaml`.

Required fields:

```yaml
created: 2024-01-15T10:30:00Z
updated: 2024-01-15T10:30:00Z
title: "Task: Database migration - Customer Platform"
entity: task
tags:
  - task
  - database
  - migration
```

Field rules:

- `created`: compatibility field in `meta.yaml`; mirror `stats.yaml` exactly
- `updated`: compatibility field in `meta.yaml`; mirror `stats.yaml` exactly
- `title`: compatibility field in `meta.yaml`; mirror `stats.yaml` exactly
- `entity`: compatibility field in `meta.yaml`; mirror `stats.yaml` exactly
- `tags`: compatibility field in `meta.yaml`; mirror `stats.yaml` exactly

Optional fields may be added (for example: `owner`, `priority`, `date`).

## System Notes

System notes are internal KEG infrastructure nodes (indexes, entity definitions, config-related nodes). They must be tagged with `system`. Exclude them from normal discovery with `tap list --query "not system"`. Do not create system notes unless explicitly requested.

## Tag Strategy

Use consistent categories such as:

- Special: `system` (reserved for internal KEG infrastructure nodes)
- Entity/type: `task`, `pr`, `patch`, `reference`, `meeting`, `project`, `client`, `issue`, `article`, `spike`, `concept`, `plan`, `hardware`, `gear`, `software`
- Technology: `python`, `javascript`, `database`, `api`, `frontend`, `backend`, `devops`, `golang`
- Work type: `bugfix`, `feature`, `refactor`, `documentation`, `testing`, `infrastructure`
- Status (optional): `in-progress`, `blocked`, `reviewed`, `deployed`

## Node Type Conventions

- `task`: `[ ] Task: Title - Context` (`[ ]` / `[x]` for status)
- `pr`: `PR: Title - Context`
- `patch`: `Patch: Title - Context`
- `reference`: `Reference: Title (slug)`
- `meeting`: `Meeting: Title - Date`
- `event`: `Event: Title - Date`
- `project`: `Project: Title (slug)`
- `client`: `Client: Name (slug)`
- `article`: `Article: Title - Source`
- `issue`: `Issue: Title - Context`
- `spike`: `Spike: Title - Context`
- `concept`: `Concept: Title`
- `plan`: `Plan: Title - Context`
- `hardware`: `Hardware: Title - Context`
- `gear`: `Gear: Title - Context`
- `software`: `Software: Title - Context`

## Checklist Before Commit

- H1 is first line and singular
- Lead paragraph exists below H1
- Internal links use `../NODEID`
- Links to other KEG notes use `keg:KEG/NODEID`
- `stats.yaml` includes all required fields
- `stats.yaml` title matches README H1 exactly
- `stats.yaml` is updated manually until tooling is available
- `meta.yaml` includes all required compatibility fields
- `meta.yaml` compatibility fields (`created`, `updated`, `title`, `entity`, `tags`) mirror `stats.yaml`
- Tags are lowercase and hyphen-separated
- `created`/`updated` are ISO 8601 UTC with trailing `Z`
- Content excludes credentials or sensitive values

## Useful Commands

All file access and editing must go through `tap`. Do not directly read or write node files. Add `-k KEG` when targeting a non-default keg.

```bash
tap repo list
tap list
tap list --query EXPR
tap tags
tap tags TAG
tap cat NODEID
tap backlinks NODEID
tap grep QUERY
CONTENT | tap create
tap edit NODEID
tap meta NODEID --edit
tap config
tap config edit
tap reindex
```

## Query Expressions

Several commands accept `--query EXPR` to filter nodes with boolean expressions. Run `tap docs query-expressions` for full syntax and examples.

## Node Creation Workflow

1. Prepare content with H1 title, lead paragraph, and frontmatter tags, then `CONTENT | tap create`
2. Use `tap edit NODEID` to refine content (H1 + lead paragraph + sections)
3. Use `tap meta NODEID --edit` to adjust metadata if needed
4. Commit node files
5. Rebuild index (`tap reindex`) when needed and commit index outputs

## Interlinking Best Practices

When adding or revising links, follow the canonical guides:

- [Guide: Best practices for interlinking a KEG](keg:pub/921) — linking conventions and anti-patterns
- [Guide: Interlink nodes in a KEG](keg:pub/1257) — step-by-step interlinking workflow

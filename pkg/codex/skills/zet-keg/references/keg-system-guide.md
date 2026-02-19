# KEG System Guide

## Overview

A KEG (Knowledge Exchange Graph) is a zettelkasten-style note repository in Git. Each node is a numbered directory with markdown content and YAML metadata.

Core characteristics:
- Version controlled in Git
- Numbered node directories: `docs/1/`, `docs/2/`, ...
- Node files: `README.md`, `stats.yaml`, and `meta.yaml`
- `meta.yaml` stores user-edited metadata; `stats.yaml` stores operational fields managed by tools
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

`stats.yaml` is the source of truth for `created`, `updated`, `title`, and `entity`. Tooling is expected to manage this file, but update it manually until those tools are available.

Required fields:

```yaml
created: 2024-01-15T10:30:00Z
updated: 2024-01-15T10:30:00Z
title: "Task: Database migration - Customer Platform"
entity: task
```

Field rules:
- `created`: set once at creation; never change
- `updated`: set to current UTC ISO 8601 timestamp for meaningful edits
- `title`: must match README H1 exactly (including checkbox status if present)
- `entity`: lowercase category

## meta.yaml Rules (User-Edited Data)

Use it for user-edited metadata (for example `tags` and optional user-managed fields).

Keep fields `created`, `updated`, `title`, and `entity` present for compatibility, but treat those four fields as deprecated within `meta.yaml` and source them from `stats.yaml`.

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
- `created`: deprecated in `meta.yaml`; keep for compatibility and mirror `stats.yaml` exactly
- `updated`: deprecated in `meta.yaml`; keep for compatibility and mirror `stats.yaml` exactly
- `title`: deprecated in `meta.yaml`; keep for compatibility and mirror `stats.yaml` exactly
- `entity`: deprecated in `meta.yaml`; keep for compatibility and mirror `stats.yaml` exactly
- `tags`: lowercase, hyphen-separated discovery tokens

Optional fields may be added (for example: `owner`, `priority`, `date`).

## Tag Strategy

Use consistent categories such as:
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
- `meta.yaml` includes all required fields
- `meta.yaml` compatibility fields (`created`, `updated`, `title`, `entity`) mirror `stats.yaml`
- Tags are lowercase and hyphen-separated
- `created`/`updated` are ISO 8601 UTC with trailing `Z`
- Content excludes credentials or sensitive values

## Useful Commands

Use these as available in your environment:

```bash
zet KEG list
zet KEG tags
zet KEG tags TAG
zet KEG cat NODEID
zet KEG backlinks NODEID
zet KEG grep QUERY
zet KEG index
```

## Node Creation Workflow

1. Create node directory: `docs/NODEID/`
2. Write `README.md` with H1 + lead paragraph + sections
3. Write `stats.yaml` with required fields (`created`, `updated`, `title`, `entity`) and update it manually for now
4. Write `meta.yaml` with required fields, keeping deprecated compatibility fields mirrored from `stats.yaml`
5. Commit node files
6. Rebuild index (`zet index`) when needed and commit index outputs

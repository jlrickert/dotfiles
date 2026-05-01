---
name: git-commit
description: Draft Git commit messages in conventional commit format without scope. Use when creating or rewriting commit messages, preparing git commit text, or generating git-cliff-friendly changelog entries. Supports optional Asana, GitHub issue, and Bitbucket issue references in a footer.
---

# Git Commit

Create commit messages that are clear, changelog-friendly, and consistent.

## Output Contract

- Subject format: `<type>: <description>`
- Subject rules:
  - Use one of: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`, `security`
  - Omit optional scope
  - Keep subject under 72 characters
  - Use present tense and imperative style
- Body rules:
  - Explain why the change exists, not implementation details
  - Wrap lines at 100 characters
  - Keep wording concise and plain
  - Do not use em dashes
- Footer references:
  - Add references only when links are available
  - Use the exact line format: `Related to <url>`
  - Order links as Asana first, then GitHub issues, then Bitbucket issues

## Workflow

1. Determine change intent and pick the narrowest valid type.
2. Draft a subject that describes outcome at a high level.
3. Trim subject length to 71 characters or fewer.
4. Write a body that explains reason and impact.
5. Add optional footer links in the required order.
6. Return the final message in a `gitcommit` fenced block.

## Type Selection

- `feat`: add user-visible capability or behavior
- `fix`: correct a defect or restore expected behavior
- `docs`: update documentation only
- `style`: formatting changes only, no behavior change
- `refactor`: restructure code without behavior change
- `perf`: improve performance or resource use
- `test`: add or adjust tests
- `chore`: maintenance, tooling, or housekeeping
- `security`: reduce risk or harden security behavior

## Link Handling

- Include each discovered link once.
- If no links are available, omit footer lines.
- Keep each reference as a single line.

Examples:

- `Related to https://app.asana.com/<redacted>`
- `Related to https://github.com/org/repo/issues/123`
- `Related to https://bitbucket.org/org/repo/issues/456`

## Quality Checks

1. Subject starts with a valid type and `:`.
2. Subject does not use scope syntax such as `type(scope):`.
3. Subject length is below 72 characters.
4. Body explains why and impact, not implementation details.
5. Body lines wrap at 100 characters.
6. Footer references use `Related to <url>` and correct ordering.

## Example

```gitcommit
fix: align keg resolution defaults across project and user config

Use project defaults first to make command behavior predictable when both project and user
configuration are present.

Related to https://app.asana.com/<redacted>
```

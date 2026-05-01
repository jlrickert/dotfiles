---
name: yq
description: Query, transform, and convert YAML files using yq (v4). Use when reading, updating, merging, or converting YAML/JSON/TOML/XML, including front-matter extraction from markdown files.
---

# yq — YAML Processing

Process YAML (and JSON/TOML/XML) from the command line with Mike Farah's yq v4.

## Core Patterns

```bash
# Read a value
yq '.key.nested' file.yaml

# Update in place
yq -i '.key = "value"' file.yaml

# Pipe from stdin
echo 'foo: bar' | yq '.foo'

# Create from scratch
yq -n '.name = "demo" | .version = 1'

# Evaluate multiple expressions
yq -i '(.a = 1) | (.b = 2)' file.yaml
```

## Common Operations

### Get / Set / Delete

```bash
yq '.metadata.name' file.yaml
yq -i '.metadata.name = "new"' file.yaml
yq -i 'del(.metadata.annotations)' file.yaml
```

### Arrays

```bash
yq '.items[0]' file.yaml
yq -i '.items += ["new"]' file.yaml
yq -i '.items |= map(select(. != "remove"))' file.yaml
yq 'length' file.yaml                         # count root keys or array items
```

### Merge

```bash
yq '. *= load("override.yaml")' base.yaml
yq eval-all 'select(fileIndex == 0) * select(fileIndex == 1)' a.yaml b.yaml
```

### Conditional Update

```bash
yq -i '(.items[] | select(.name == "target")).value = "patched"' file.yaml
```

## Format Conversion

```bash
# YAML -> JSON
yq -o json file.yaml

# JSON -> YAML
yq -p json file.json

# JSON -> YAML (explicit output)
yq -p json -o yaml file.json

# YAML -> TOML
yq -o toml file.yaml

# TOML -> YAML
yq -p toml file.toml

# YAML -> XML
yq -o xml file.yaml

# XML -> YAML
yq -p xml file.xml
```

## Useful Flags

| Flag | Purpose |
|------|---------|
| `-i` | Edit file in place |
| `-n` | Null input — start from scratch |
| `-o FORMAT` | Output format: `yaml`, `json`, `toml`, `xml`, `csv`, `tsv`, `props` |
| `-p FORMAT` | Parse input as format (same options as `-o`) |
| `-P` | Pretty print |
| `-N` | No colors |
| `-e` | Exit with error on missing key |
| `-f FILE` | Read expression from file |
| `--header-preprocess` | Pre-process headers (enabled by default) |
| `--front-matter` | Extract/process YAML front-matter (`extract`, `process`) |

## Expression Syntax

### Navigation

```bash
.key                  # map access
.key.nested           # nested access
.items[0]             # array index
.items[-1]            # last element
.items[]              # iterate array
.a, .b                # multiple keys
```

### Wildcards and Recursion

```bash
.items[].name         # all names in array
..                    # recursive descent
.[] | key             # all keys at current level
```

### Select / Filter

```bash
select(.age > 30)
select(.name == "target")
select(.tags[] == "important")
select(has("optional"))
```

### Assignment

```bash
.key = "value"
.key |= . + 1
.key //= "default"    # set only if null or missing
```

### Functions

```bash
length                # count keys or array elements
keys                  # list map keys
values                # list map values
type                  # show type (!!map, !!seq, !!str, etc.)
sort_by(.name)        # sort array of maps
group_by(.category)   # group array of maps
unique                # deduplicate array
flatten               # flatten nested arrays
to_entries             # convert map to key/value pairs
from_entries           # convert key/value pairs to map
env(VAR)              # read environment variable
strenv(VAR)           # read env var as string
```

### Comments

```bash
# Read a comment
yq '.key | head_comment' file.yaml
yq '.key | line_comment' file.yaml
yq '.key | foot_comment' file.yaml

# Set a comment
yq -i '.key head_comment = "above"' file.yaml
yq -i '.key line_comment = "beside"' file.yaml
```

## KEG Configuration through `tap config edit`

KEG config output from `tap config` can be large. Dump it to a temp file, use
yq to manipulate it, then pipe the result back through `tap config edit`:

```bash
# Dump current config to a temp file
tap config > /tmp/keg-config.yaml

# Read or query the config
yq '.entities' /tmp/keg-config.yaml

# Modify and pipe back
yq '.some.setting = "new-value"' /tmp/keg-config.yaml | tap config edit

# Or modify in place first, then pipe
yq -i '.some.setting = "new-value"' /tmp/keg-config.yaml
cat /tmp/keg-config.yaml | tap config edit

# Clean up
rm /tmp/keg-config.yaml
```

## Front-matter Processing

Extract or update YAML front-matter in markdown files:

```bash
# Extract front-matter as YAML
yq --front-matter=extract '.title' post.md

# Process front-matter in place (preserves body)
yq -i --front-matter=process '.tags += ["new"]' post.md
```

## Built-in Documentation

```bash
yq --help              # general usage
yq eval --help         # expression evaluation help
```

Upstream docs: https://mikefarah.gitbook.io/yq/

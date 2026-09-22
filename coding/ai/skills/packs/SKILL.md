---
name: packs
description: Create Logtalk packs and pack registries (pack_protocol, registry_protocol, version metadata, checksums, loaders). Use when authoring a distributable library or application pack, adding it to a registry, or publishing pack specs.
---

# Packs Skill

Use the official `packs` tool and protocols to define and publish packs. Do not invent a parallel packaging scheme.

## Authoritative documentation

Read and follow:

- https://logtalk.org/handbook/devtools/packs.html

Protocols and API: see the packs tool API docs linked from that page. Public registry list: https://github.com/LogtalkDotOrg/pack-registries

Do not duplicate the handbook. Prefer linking to specific sections.

## When to use this skill

- Turning a Logtalk library or application into an installable pack
- Writing a pack specification object (`*_pack`)
- Creating or updating a pack registry (`*_registry`, loader, listing packs)
- Adding versions, checksums, dependencies, licenses, and archive URLs
- Local development/testing of a registry via `file://` URLs

## Core concepts

- **Pack** — distributable library/application with a specification object implementing `pack_protocol`.
- **Registry** — collection of pack specs (git repo, archive, or local directory) with a registry object implementing `registry_protocol` and a `loader.lgt`.
- **Storage** — under `logtalk_packs` / `LOGTALKPACKS` / `~/logtalk_packs` (virtual environments supported).

Load the tool:

```logtalk
| ?- logtalk_load(packs(loader)).
```

## Pack specification (authoring)

1. **Object and file naming**
   - Object implements `pack_protocol`.
   - File/object name: pack name + `_pack` suffix (e.g. `lflat_pack` in `lflat_pack.lgt`) to avoid clashes with the pack’s own entities.

2. **Required metadata** (see handbook examples)
   - `name/1`, `description/1`, `license/1` (prefer SPDX ids), `home/1`
   - One or more `version/6` clauses:
     `version(Version, Status, URL, Checksum, Dependencies, Portability)`
   - Version term: `Major:Minor:Patch`
   - Checksum: `sha256 - '...'` over the archive
   - Dependencies: e.g. `[logtalk @>= 3:42:0]` and other pack constraints as documented
   - Portability: e.g. `all` or as specified in the protocol docs

3. **Pack sources (the archive contents)**
   - Include `LICENSE`, `README.md` (or `NOTES.md`), `loader.lgt` (or `.logtalk`)
   - Strongly prefer a `tester.lgt` so users and CI can run tests after install
   - Follow normal Logtalk project layout (coding-style / libraries skills)

4. **Archives**
   - Supported: `.zip`, `.tgz`/`.tar.gz`, `.tbz2`/`.tar.bz2` (and `.gpg` variants when encrypted)
   - Publish a stable download URL per version (often a git tag archive URL)
   - Compute and publish the SHA-256 checksum matching that exact archive

5. **Optional**
   - `note(Action, Version, Note)` for install/update/uninstall messages
   - Detached signatures (`.asc` / `.sig`) and encryption per handbook

## Registry specification (publishing packs)

1. **Registry object**
   - Implements `registry_protocol`; name ends with `_registry`.
   - Define `name/1`, `description/1`, `home/1`, and typically `clone/1` and/or `archive/1`.

2. **Registry directory layout**
   ```text
   my_registry/
       LICENSE
       README.md
       my_registry_registry.lgt
       loader.lgt
       foo_pack.lgt
       bar_pack.lgt
   ```
   - `loader.lgt` loads the registry object and every pack spec object (one file per object preferred for tool sanitization).

3. **Development**
   - Add a local registry with a `file://` URL while iterating.
   - Run `registries::lint/0-1` after adding.
   - Prefer git hosting for real distribution; HTTPS `.git` clone URLs simplify updates.

4. **Users install via**
   ```logtalk
   | ?- logtalk_load(packs(loader)).
   | ?- registries::add('https://example.com/my_registry.git').
   | ?- packs::install(Registry, Pack).   % see packs API for exact install predicates
   ```
   Exact install/update/uninstall messages: handbook and `packs::help`, `registries::help`.

## Checklist for a new pack release

- [ ] Pack object implements `pack_protocol` and uses the `_pack` naming convention
- [ ] New `version/6` entry with correct URL, `sha256`, dependencies, portability
- [ ] Archive contents include LICENSE, README/NOTES, loader; tests recommended
- [ ] Registry loader lists the pack spec; registry lint clean
- [ ] Install from a clean/virtual env and run `tester.lgt` after install
- [ ] License SPDX where possible; home URL valid

## What to avoid

- Inventing install scripts that bypass `packs` / `registries`
- Putting pack specs and application entities in one file in a way that breaks tool expectations (prefer one object per file for registry/pack specs)
- Wrong or outdated checksums (must match the published archive bytes)
- Omitting `loader.lgt` or license files from the pack archive
- Registry names/objects that collide with pack or application entities (use `_registry` / `_pack` suffixes)

## Further reading

- Packs tool: https://logtalk.org/handbook/devtools/packs.html
- Public registries: https://github.com/LogtalkDotOrg/pack-registries
- Example registry/packs (e.g. talkshow): https://github.com/LogtalkDotOrg/talkshow
- Virtual environments, save/restore, lock files: same handbook page

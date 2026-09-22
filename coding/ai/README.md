
## Core Files

### `AGENTS.md`
The single source of truth for project-level context. It tells AI agents:

- What Logtalk is and how it relates to Prolog
- Preferred entity kinds (objects, categories, protocols) and when to use them
- File and directory conventions (loaders, testers, packs, libraries)
- Build, load, and test commands
- Coding style, idioms, and common anti-patterns
- Pointers to the official documentation and examples

Keep this file concise. Detailed procedures belong in skills.

### Skills (`skills/*/SKILL.md`)
Portable, on-demand task knowledge following the Agent Skills standard. Each skill is a directory containing at least a `SKILL.md` file with YAML frontmatter (`name` + `description`) and a clear procedure.

Current skills cover common Logtalk workflows such as:

- Following coding guidelines
- Writing and running tests with `lgtunit`
- Performance (indexing, determinism, tail recursion)
- Reusing standard libraries
- Debugging applications
- Documenting entities
- Writing portable applications and libraries
- Authoring packs and pack registries

Skills are loaded only when relevant, keeping the main context window clean.

### Agents (`agents/`)
Definitions for specialized sub-agents (tester, documenter, reviewer, etc.). These are more tool-specific. Copy or adapt them into the location expected by your tool (e.g. `.claude/agents/`, `.github/agents/`).

## Tool-Specific Adapters

| Tool              | Recommended file                          | Notes |
|-------------------|-------------------------------------------|-------|
| Most tools        | `AGENTS.md` (root of project)             | Preferred cross-tool standard |
| Claude Code       | `CLAUDE.md` (can `@import` AGENTS.md)     | Also supports `.claude/skills/` and `.claude/agents/` |
| Cursor            | `AGENTS.md` or `.cursor/rules/`           | Skills can live under `.cursor/skills/` |
| GitHub Copilot    | `AGENTS.md` + `.github/copilot-instructions.md` | Custom agents under `.github/agents/` |
| Others            | `AGENTS.md`                               | Many tools already look for it |

You can keep a single canonical `AGENTS.md` and create thin adapters (or symlinks) for tools that prefer a different filename.

## How to Add a New Skill

1. Create a new directory under `skills/`, e.g. `skills/my-task/`.
2. Add a `SKILL.md` with:
   - YAML frontmatter containing at least `name` and `description`
   - Clear “When to use" and step-by-step procedure
   - Logtalk-specific conventions and verification steps
3. Optionally add `references/`, `examples/`, or `scripts/`.
4. Keep the skill focused on one coherent task.

## Philosophy

- **AGENTS.md** answers “What is this project and how should I work here?"
- **Skills** answer “How do I perform this specific task well in Logtalk?"
- **Agents** provide focused personas or isolated context for particular roles.

Prefer short, actionable instructions over long narrative. Update these files when Logtalk conventions or tooling change.

## Contributing

Improvements to the guidance, new skills, or better examples are welcome. Please keep changes focused and consistent with the existing style.

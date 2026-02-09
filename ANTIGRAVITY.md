# Google Antigravity Agent Custom Instructions

For all tasks in this project, you MUST follow the instructions in [AGENTS.md](AGENTS.md) and its referenced files.

## Primary References

- **Master Instructions**: [AGENTS.md](AGENTS.md)
- **Guardrails**: [buildSrc/.agent/guardrails.md](buildSrc/.agent/guardrails.md)
- **Code Style**: [.agents/code-convention.md](.agents/code-convention.md)
- **Workflows**: [buildSrc/.agent/development-workflow.md](buildSrc/.agent/development-workflow.md)
- **Commands**: [buildSrc/.agent/common-commands.md](buildSrc/.agent/common-commands.md)

## Summary of Critical Rules

1. **Always read AGENTS.md** at the start of a session.
2. **Configure MCP** if needed (see `.mcp.json`).
3. **Run tests** frequently.
4. **Do not use `???`** placeholders.
5. **Format code** using `scalafmt`.
6. **Commit often** with message prefix `!amend [AI]`.

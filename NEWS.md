# claudeAgentR 0.1.1

* Aligns SDK session handling with the current Claude CLI by resuming
  conversations with Claude `session_id` instead of relying on a long-lived
  subprocess.
* Updates transport startup to pass prompts as CLI arguments and include the
  required `--verbose` flag for `stream-json` output.
* Fixes cost parsing to read `total_cost_usd` from result messages.
* Improves sync/async client recovery and adds test coverage for session resume
  behavior and current CLI result schema.

# claudeAgentR 0.1.0

Initial release.

* `claude_query()` and `claude_execute()` — one-shot query functions
* `ClaudeClient` / `claude_client()` — synchronous multi-turn client
* `ClaudeAsyncClient` / `claude_async_client()` — promise-based async client
* `HookRegistry` — pre- and post-tool-use hook system
* `ClaudeOptions` — unified configuration object
* MCP server support via `mcp_servers` parameter
* Configurable `timeout`, `max_tokens`, and `permission_mode` validation

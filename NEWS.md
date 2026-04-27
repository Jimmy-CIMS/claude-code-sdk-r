# claudeAgentR 0.1.0

Initial release.

* `claude_query()` and `claude_execute()` — one-shot query functions
* `ClaudeClient` / `claude_client()` — synchronous multi-turn client
* `ClaudeAsyncClient` / `claude_async_client()` — promise-based async client
* `HookRegistry` — pre- and post-tool-use hook system
* `ClaudeOptions` — unified configuration object
* MCP server support via `mcp_servers` parameter
* Configurable `timeout`, `max_tokens`, and `permission_mode` validation

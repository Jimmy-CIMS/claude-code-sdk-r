# claudeAgentR

An R SDK for programmatically interacting with [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code).

## Requirements

- R >= 4.1.0
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code) installed (`npm install -g @anthropic-ai/claude-code`)
- R packages: `R6`, `processx`, `jsonlite`, `cli`, `promises`, `later`

## Installation

```r
# Install from source
pak::pak("path/to/claudeAgentR")
```

## Quick Start

### One-shot query (simplest)

```r
library(claudeAgentR)

answer <- claude_query("What is 2 + 2?")
cat(answer)
```

### Full metadata

```r
res <- claude_execute(
  "Write a haiku about R programming.",
  model = "claude-sonnet-4-20250514"
)
cat(res$text)
cat("Cost: $", res$cost_usd, "\n")
```

### Multi-turn conversation (synchronous)

```r
client <- claude_client(
  model       = "claude-sonnet-4-20250514",
  working_dir = "."
)

cat(client$connect_text("What is 2 + 2?"))
cat(client$query_text("Multiply that by 10"))

client$close()
```

Use `with_claude()` for automatic cleanup:

```r
result <- with_claude(claude_client(), {
  client$connect_text("Summarise the mtcars dataset in one sentence.")
})
```

Each turn starts a fresh `claude --print` process and resumes the prior Claude
session automatically.

### Asynchronous client (Shiny / event-loop)

```r
library(promises)

client <- claude_async_client(model = "claude-sonnet-4-20250514")

p <- client$connect_text("What is 2 + 2?")
p %...>% cat()

later::run_now(timeout = 30)
```

Follow-up turns use the same API:

```r
client$query_text("Multiply that by 10") %...>% cat()
later::run_now(timeout = 30)
```

### Tool interception with hooks

```r
hooks <- HookRegistry$new()

# Block dangerous commands
hooks$register_pre_tool_use("Bash", function(input) {
  if (grepl("rm -rf", input$command %||% "")) {
    hook_block("Dangerous command blocked")
  } else {
    hook_allow()
  }
})

# Log file writes
hooks$register_post_tool_use("Write", function(ctx) {
  message("File written: ", ctx$input$file_path)
})

client <- claude_client(hooks = hooks)
```

### MCP server integration

```r
client <- claude_client(
  mcp_servers = list(
    my_server = list(
      command = "node",
      args    = list("/path/to/mcp-server.js"),
      env     = list(API_KEY = "secret")
    )
  )
)
```

## Configuration

`ClaudeOptions` controls all settings:

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `model` | character | CLI default | Model identifier |
| `system_prompt` | character | `NULL` | System prompt override |
| `permission_mode` | character | `"default"` | `"default"`, `"acceptEdits"`, or `"bypassPermissions"` |
| `allowed_tools` | character vector | `NULL` | Whitelist of tools |
| `disallowed_tools` | character vector | `NULL` | Blacklist of tools |
| `max_turns` | integer | `NULL` | Maximum agentic turns |
| `max_tokens` | integer | `NULL` | Maximum tokens per response |
| `timeout` | numeric | `300` | Seconds before killing subprocess |
| `working_dir` | character | `getwd()` | Working directory for CLI |
| `mcp_servers` | named list | `NULL` | MCP server configurations |

## License

Apache License 2.0

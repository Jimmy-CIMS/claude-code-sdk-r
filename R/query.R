#' One-shot Claude query (simplest API)
#'
#' @description
#' The easiest way to call Claude: provide a prompt, get back a text response.
#' Spawns Claude CLI, collects the full response, and terminates the process.
#'
#' This mirrors `Query.text()` from the Java SDK and `query()` from the Python SDK.
#'
#' @param prompt Character string prompt.
#' @param model Model identifier. Defaults to Claude's own default.
#' @param system_prompt Optional system prompt string.
#' @param permission_mode One of `"default"`, `"acceptEdits"`,
#'   `"bypassPermissions"`.
#' @param allowed_tools Character vector of allowed tool names.
#' @param max_turns Integer maximum turns.
#' @param max_tokens Integer maximum tokens per response.
#' @param timeout Numeric seconds before the subprocess is killed.
#' @param working_dir Working directory for the CLI (defaults to `getwd()`).
#' @param mcp_servers Named list of MCP server configurations.
#' @param on_message Optional `function(msg)` called for each streamed message.
#' @return Character string of the assistant's response.
#'
#' @examples
#' \dontrun{
#' answer <- claude_query("What is 2 + 2?")
#' cat(answer)
#'
#' # With options
#' answer <- claude_query(
#'   "Summarise the mtcars dataset.",
#'   system_prompt = "Be concise. One paragraph.",
#'   model = "claude-sonnet-4-20250514"
#' )
#' }
#'
#' @export
claude_query <- function(
  prompt,
  model           = NULL,
  system_prompt   = NULL,
  permission_mode = "default",
  allowed_tools   = NULL,
  max_turns       = NULL,
  max_tokens      = NULL,
  timeout         = 300,
  working_dir     = NULL,
  mcp_servers     = NULL,
  on_message      = NULL
) {
  opts <- ClaudeOptions$new(
    model           = model,
    system_prompt   = system_prompt,
    permission_mode = permission_mode,
    allowed_tools   = allowed_tools,
    max_turns       = max_turns,
    max_tokens      = max_tokens,
    timeout         = timeout,
    working_dir     = working_dir,
    mcp_servers     = mcp_servers
  )

  proc <- start_transport(prompt, opts)
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  result <- collect_messages(proc, hooks = NULL, on_message = on_message, timeout = timeout)
  result$text
}

#' One-shot Claude query returning full result metadata
#'
#' @description
#' Like `claude_query()` but returns a list with `text`, `messages`, `cost_usd`,
#' and `subtype` fields.
#'
#' @inheritParams claude_query
#' @return A list with fields:
#'   * `text` — character string of assistant response
#'   * `messages` — list of all parsed messages
#'   * `cost_usd` — numeric cost in USD (if available), or `NA`
#'   * `subtype` — result subtype string, or `NA`
#'
#' @examples
#' \dontrun{
#' res <- claude_execute("Write a haiku about R programming.")
#' cat(res$text)
#' cat("Cost: $", res$cost_usd, "\n")
#' }
#'
#' @export
claude_execute <- function(
  prompt,
  model           = NULL,
  system_prompt   = NULL,
  permission_mode = "default",
  allowed_tools   = NULL,
  max_turns       = NULL,
  max_tokens      = NULL,
  timeout         = 300,
  working_dir     = NULL,
  mcp_servers     = NULL,
  on_message      = NULL
) {
  opts <- ClaudeOptions$new(
    model           = model,
    system_prompt   = system_prompt,
    permission_mode = permission_mode,
    allowed_tools   = allowed_tools,
    max_turns       = max_turns,
    max_tokens      = max_tokens,
    timeout         = timeout,
    working_dir     = working_dir,
    mcp_servers     = mcp_servers
  )

  proc <- start_transport(prompt, opts)
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  result <- collect_messages(proc, hooks = NULL, on_message = on_message, timeout = timeout)

  cost    <- result$result$cost_usd %||% NA_real_
  subtype <- result$result$subtype  %||% NA_character_

  list(
    text     = result$text,
    messages = result$messages,
    cost_usd = cost,
    subtype  = subtype
  )
}

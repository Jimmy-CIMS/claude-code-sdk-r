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
#' @param disallowed_tools Character vector of disallowed tool names.
#' @param tools Character vector of base enabled tools.
#' @param max_turns Integer maximum turns.
#' @param max_tokens Integer maximum tokens per response.
#' @param max_thinking_tokens Integer maximum thinking tokens.
#' @param timeout Numeric seconds before the subprocess is killed.
#' @param working_dir Working directory for the CLI (defaults to `getwd()`).
#' @param mcp_servers Named list of MCP server configurations.
#' @param append_system_prompt Text appended to the system prompt.
#' @param max_budget_usd Numeric maximum budget in USD.
#' @param fallback_model Fallback model identifier.
#' @param setting_sources Character vector of setting sources.
#' @param add_dirs Character vector of additional directories.
#' @param settings Custom settings file path.
#' @param agents JSON string describing subagents.
#' @param json_schema Named list JSON schema for structured output.
#' @param include_partial_messages Whether to include partial stream messages.
#' @param continue_conversation Whether to pass `--continue`.
#' @param fork_session Whether to pass `--fork-session`.
#' @param permission_prompt_tool_name Permission prompt tool name.
#' @param plugins Character vector of plugin directories.
#' @param extra_args Named list/vector of extra CLI flags.
#' @param env Named environment overrides for the subprocess.
#' @param max_buffer_size Maximum accepted stream JSON line size in bytes.
#' @param user Optional Unix user to execute via `sudo -u`.
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
  disallowed_tools = NULL,
  tools           = NULL,
  max_turns       = NULL,
  max_tokens      = NULL,
  max_thinking_tokens = NULL,
  timeout         = 300,
  working_dir     = NULL,
  mcp_servers     = NULL,
  append_system_prompt = NULL,
  max_budget_usd  = NULL,
  fallback_model  = NULL,
  setting_sources = NULL,
  add_dirs        = NULL,
  settings        = NULL,
  agents          = NULL,
  json_schema     = NULL,
  include_partial_messages = FALSE,
  continue_conversation = FALSE,
  fork_session    = FALSE,
  permission_prompt_tool_name = NULL,
  plugins         = NULL,
  extra_args      = NULL,
  env             = NULL,
  max_buffer_size = 1024 * 1024,
  user            = NULL,
  on_message      = NULL
) {
  opts <- ClaudeOptions$new(
    model           = model,
    system_prompt   = system_prompt,
    append_system_prompt = append_system_prompt,
    permission_mode = permission_mode,
    allowed_tools   = allowed_tools,
    disallowed_tools = disallowed_tools,
    tools           = tools,
    max_turns       = max_turns,
    max_tokens      = max_tokens,
    max_thinking_tokens = max_thinking_tokens,
    timeout         = timeout,
    working_dir     = working_dir,
    mcp_servers     = mcp_servers,
    max_budget_usd  = max_budget_usd,
    fallback_model  = fallback_model,
    setting_sources = setting_sources,
    add_dirs        = add_dirs,
    settings        = settings,
    agents          = agents,
    json_schema     = json_schema,
    include_partial_messages = include_partial_messages,
    continue_conversation = continue_conversation,
    fork_session    = fork_session,
    permission_prompt_tool_name = permission_prompt_tool_name,
    plugins         = plugins,
    extra_args      = extra_args,
    env             = env,
    max_buffer_size = max_buffer_size,
    user            = user
  )

  proc <- start_transport(prompt, opts)
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  result <- collect_messages(
    proc,
    hooks = NULL,
    on_message = on_message,
    timeout = timeout,
    max_buffer_size = opts$max_buffer_size
  )
  result$text
}

#' One-shot Claude query returning full result metadata
#'
#' @description
#' Like `claude_query()` but returns a list with `text`, `messages`, `cost_usd`,
#' `subtype`, and `session_id` fields.
#'
#' @inheritParams claude_query
#' @return A list with fields:
#'   * `text` — character string of assistant response
#'   * `messages` — list of all parsed messages
#'   * `cost_usd` — numeric cost in USD (if available), or `NA`
#'   * `subtype` — result subtype string, or `NA`
#'   * `session_id` — Claude session id, or `NA`
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
  disallowed_tools = NULL,
  tools           = NULL,
  max_turns       = NULL,
  max_tokens      = NULL,
  max_thinking_tokens = NULL,
  timeout         = 300,
  working_dir     = NULL,
  mcp_servers     = NULL,
  append_system_prompt = NULL,
  max_budget_usd  = NULL,
  fallback_model  = NULL,
  setting_sources = NULL,
  add_dirs        = NULL,
  settings        = NULL,
  agents          = NULL,
  json_schema     = NULL,
  include_partial_messages = FALSE,
  continue_conversation = FALSE,
  fork_session    = FALSE,
  permission_prompt_tool_name = NULL,
  plugins         = NULL,
  extra_args      = NULL,
  env             = NULL,
  max_buffer_size = 1024 * 1024,
  user            = NULL,
  on_message      = NULL
) {
  opts <- ClaudeOptions$new(
    model           = model,
    system_prompt   = system_prompt,
    append_system_prompt = append_system_prompt,
    permission_mode = permission_mode,
    allowed_tools   = allowed_tools,
    disallowed_tools = disallowed_tools,
    tools           = tools,
    max_turns       = max_turns,
    max_tokens      = max_tokens,
    max_thinking_tokens = max_thinking_tokens,
    timeout         = timeout,
    working_dir     = working_dir,
    mcp_servers     = mcp_servers,
    max_budget_usd  = max_budget_usd,
    fallback_model  = fallback_model,
    setting_sources = setting_sources,
    add_dirs        = add_dirs,
    settings        = settings,
    agents          = agents,
    json_schema     = json_schema,
    include_partial_messages = include_partial_messages,
    continue_conversation = continue_conversation,
    fork_session    = fork_session,
    permission_prompt_tool_name = permission_prompt_tool_name,
    plugins         = plugins,
    extra_args      = extra_args,
    env             = env,
    max_buffer_size = max_buffer_size,
    user            = user
  )

  proc <- start_transport(prompt, opts)
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  result <- collect_messages(
    proc,
    hooks = NULL,
    on_message = on_message,
    timeout = timeout,
    max_buffer_size = opts$max_buffer_size
  )

  cost       <- result$result$total_cost_usd %||% result$result$cost_usd %||% NA_real_
  subtype    <- result$result$subtype %||% NA_character_
  session_id <- extract_session_id(result) %||% NA_character_

  list(
    text     = result$text,
    messages = result$messages,
    result   = result$result,
    cost_usd = cost,
    subtype  = subtype,
    session_id = session_id,
    usage = result$result$usage %||% NULL,
    duration_ms = result$result$duration_ms %||% NA_integer_,
    duration_api_ms = result$result$duration_api_ms %||% NA_integer_,
    num_turns = result$result$num_turns %||% NA_integer_,
    is_error = result$result$is_error %||% NA
  )
}

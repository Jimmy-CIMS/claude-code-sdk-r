#' Synchronous Claude Agent Client
#'
#' @description
#' `ClaudeClient` provides a blocking, multi-turn conversation interface to
#' Claude Code CLI.
#'
#' Use `ClaudeClient$new()` (or the `claude_client()` helper) to create a
#' client, then call `$connect()` for the first turn and `$query()` for
#' follow-up turns. Each turn starts a fresh CLI subprocess and resumes the
#' stored Claude session id. Always call `$close()` when done, or use
#' `with_claude()`.
#'
#' @examples
#' \dontrun{
#' client <- claude_client(
#'   model = "claude-sonnet-4-20250514",
#'   working_dir = "."
#' )
#'
#' # First turn
#' cat(client$connect_text("What is 2 + 2?"))
#'
#' # Follow-up — context is preserved
#' cat(client$query_text("Multiply that by 10"))
#'
#' client$close()
#' }
#'
#' @export
ClaudeClient <- R6::R6Class(
  "ClaudeClient",

  public = list(

    #' @field last_messages All parsed messages from the most recent turn.
    last_messages = NULL,

    #' @field last_text Concatenated assistant text from the most recent turn.
    last_text = NULL,

    #' @field last_result The result/stop message from the most recent turn.
    last_result = NULL,

    #' @description Create a new `ClaudeClient`.
    #' @param options A `ClaudeOptions` object. If `NULL`, defaults are used.
    #' @param hooks A `HookRegistry` object or `NULL`.
    initialize = function(options = NULL, hooks = NULL) {
      private$options <- options %||% ClaudeOptions$new()
      private$hooks   <- hooks
    },

    #' @description
    #' Start a Claude session with an initial prompt and return all
    #' messages as a list.
    #' @param prompt Character string.
    #' @param on_message Optional `function(msg)` called for each streamed msg.
    #' @return Invisibly returns `self` (use `$last_messages` / `$last_text`).
    connect = function(prompt, on_message = NULL) {
      if (private$connected) {
        cli::cli_abort(
          "Already connected. Use {.fn $query} for follow-up turns."
        )
      }
      result <- private$run_turn(prompt, on_message, resume = FALSE)
      private$store_result(result)
      private$connected <- TRUE
      invisible(self)
    },

    #' @description
    #' Start the subprocess and return only the assistant's text (80% use case).
    #' @param prompt Character string.
    #' @return Character string.
    connect_text = function(prompt) {
      self$connect(prompt)
      self$last_text
    },

    #' @description
    #' Send a follow-up prompt; context is preserved via Claude session resume.
    #' @param prompt Character string.
    #' @param on_message Optional callback per message.
    #' @return Invisibly returns `self`.
    query = function(prompt, on_message = NULL) {
      if (!private$connected) {
        cli::cli_abort(
          "Not connected. Call {.fn $connect} first."
        )
      }
      result <- private$run_turn(prompt, on_message, resume = TRUE)
      private$store_result(result)
      invisible(self)
    },

    #' @description
    #' Send a follow-up and return only the assistant's text.
    #' @param prompt Character string.
    #' @return Character string.
    query_text = function(prompt) {
      self$query(prompt)
      self$last_text
    },

    #' @description Forget the stored Claude session and terminate any live subprocess.
    close = function() {
      if (!is.null(private$proc) && private$proc$is_alive()) {
        private$proc$kill()
      }
      private$proc <- NULL
      private$connected <- FALSE
      private$session_id <- NULL
      invisible(NULL)
    }
  ),

  private = list(
    proc      = NULL,
    options   = NULL,
    hooks     = NULL,
    connected = FALSE,
    session_id = NULL,

    store_result = function(result) {
      self$last_messages <- result$messages
      self$last_text     <- result$text
      self$last_result   <- result$result
      private$session_id <- extract_session_id(result)
    },

    run_turn = function(prompt, on_message, resume) {
      if (resume) {
        private$options$resume_session_id <- private$session_id
      } else {
        private$options$resume_session_id <- NULL
      }

      private$proc <- start_transport(prompt, private$options)
      on.exit({
        if (!is.null(private$proc) && private$proc$is_alive()) {
          private$proc$kill()
        }
        private$proc <- NULL
      }, add = TRUE)

      collect_messages(
        private$proc, private$hooks, on_message,
        timeout = private$options$timeout,
        max_buffer_size = private$options$max_buffer_size
      )
    }
  ),

  cloneable = FALSE
)

#' Create a synchronous Claude client
#'
#' @description
#' Convenience constructor wrapping `ClaudeClient$new()` with named arguments.
#'
#' @param model Model identifier string.
#' @param system_prompt System prompt override.
#' @param permission_mode One of `"default"`, `"acceptEdits"`,
#'   `"bypassPermissions"`.
#' @param allowed_tools Character vector of allowed tool names.
#' @param disallowed_tools Character vector of disallowed tool names.
#' @param max_turns Integer maximum agent turns.
#' @param max_tokens Integer maximum tokens per response.
#' @param max_thinking_tokens Integer maximum thinking tokens.
#' @param tools Character vector of base enabled tools.
#' @param timeout Numeric seconds before subprocess timeout.
#' @param working_dir Path to working directory.
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
#' @param hooks A `HookRegistry` object or `NULL`.
#' @return A `ClaudeClient` object.
#'
#' @examples
#' \dontrun{
#' client <- claude_client(model = "claude-sonnet-4-20250514")
#' cat(client$connect_text("Hello!"))
#' client$close()
#' }
#'
#' @export
claude_client <- function(
  model            = NULL,
  system_prompt    = NULL,
  permission_mode  = "default",
  allowed_tools    = NULL,
  disallowed_tools = NULL,
  max_turns        = NULL,
  max_tokens       = NULL,
  max_thinking_tokens = NULL,
  tools            = NULL,
  timeout          = 300,
  working_dir      = NULL,
  mcp_servers      = NULL,
  append_system_prompt = NULL,
  max_budget_usd   = NULL,
  fallback_model   = NULL,
  setting_sources  = NULL,
  add_dirs         = NULL,
  settings         = NULL,
  agents           = NULL,
  json_schema      = NULL,
  include_partial_messages = FALSE,
  continue_conversation = FALSE,
  fork_session     = FALSE,
  permission_prompt_tool_name = NULL,
  plugins          = NULL,
  extra_args       = NULL,
  env              = NULL,
  max_buffer_size  = 1024 * 1024,
  user             = NULL,
  hooks            = NULL
) {
  opts <- ClaudeOptions$new(
    model            = model,
    system_prompt    = system_prompt,
    append_system_prompt = append_system_prompt,
    permission_mode  = permission_mode,
    allowed_tools    = allowed_tools,
    disallowed_tools = disallowed_tools,
    max_turns        = max_turns,
    max_tokens       = max_tokens,
    max_thinking_tokens = max_thinking_tokens,
    tools            = tools,
    timeout          = timeout,
    working_dir      = working_dir,
    mcp_servers      = mcp_servers,
    max_budget_usd   = max_budget_usd,
    fallback_model   = fallback_model,
    setting_sources  = setting_sources,
    add_dirs         = add_dirs,
    settings         = settings,
    agents           = agents,
    json_schema      = json_schema,
    include_partial_messages = include_partial_messages,
    continue_conversation = continue_conversation,
    fork_session     = fork_session,
    permission_prompt_tool_name = permission_prompt_tool_name,
    plugins          = plugins,
    extra_args       = extra_args,
    env              = env,
    max_buffer_size  = max_buffer_size,
    user             = user
  )
  ClaudeClient$new(options = opts, hooks = hooks)
}

#' Use a Claude client with automatic cleanup
#'
#' @description
#' Evaluates `expr` with `client` in scope, then calls `client$close()`
#' even if an error occurs — analogous to Java's try-with-resources.
#'
#' @param client A `ClaudeClient` object.
#' @param expr Expression to evaluate.
#' @return The value of `expr`.
#'
#' @examples
#' \dontrun{
#' result <- with_claude(claude_client(), {
#'   client$connect_text("What is 2 + 2?")
#' })
#' }
#'
#' @export
with_claude <- function(client, expr) {
  on.exit(client$close(), add = TRUE)
  force(expr)
}

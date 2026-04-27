#' Claude Agent Options
#'
#' @description
#' Configuration object for Claude Agent SDK. Pass to `ClaudeClient$new()` or
#' `claude_query()` to control model, permissions, tools, and behaviour.
#'
#' @examples
#' opts <- ClaudeOptions$new(
#'   model = "claude-sonnet-4-20250514",
#'   system_prompt = "You are a helpful data analysis assistant.",
#'   max_turns = 10
#' )
#'
#' @export
ClaudeOptions <- R6::R6Class(
  "ClaudeOptions",
  public = list(

    #' @field model Claude model identifier.
    model = NULL,

    #' @field system_prompt System prompt override.
    system_prompt = NULL,

    #' @field append_system_prompt Text appended to the default system prompt.
    append_system_prompt = NULL,

    #' @field permission_mode One of `"default"`, `"acceptEdits"`,
    #'   `"bypassPermissions"`.
    permission_mode = "default",

    #' @field allowed_tools Character vector of tool names to allow
    #'   (e.g. `c("Read", "Grep")`).
    allowed_tools = NULL,

    #' @field disallowed_tools Character vector of tool names to block.
    disallowed_tools = NULL,

    #' @field max_turns Maximum number of agentic turns.
    max_turns = NULL,

    #' @field max_tokens Maximum tokens per response.
    max_tokens = NULL,

    #' @field max_thinking_tokens Maximum thinking tokens for extended thinking.
    max_thinking_tokens = NULL,

    #' @field tools Base character vector of tool names to enable. Use
    #'   `character(0)` to pass an empty tool set.
    tools = NULL,

    #' @field timeout Duration in seconds before subprocess is killed.
    timeout = 300,

    #' @field working_dir Working directory for the CLI subprocess.
    working_dir = NULL,

    #' @field mcp_servers Named list of MCP server configurations.
    #'   Each element should be a list with `command`, `args`, and optional
    #'   `env` fields, keyed by server name.
    mcp_servers = NULL,

    #' @field resume_session_id Existing Claude session id to resume.
    resume_session_id = NULL,

    #' @field continue_conversation Continue the most recent Claude conversation.
    continue_conversation = FALSE,

    #' @field fork_session Fork a resumed session into a new session id.
    fork_session = FALSE,

    #' @field include_partial_messages Include partial streaming messages if
    #'   supported by the Claude CLI.
    include_partial_messages = FALSE,

    #' @field agents JSON string describing subagents for the Task tool.
    agents = NULL,

    #' @field json_schema Named list JSON schema for structured output.
    json_schema = NULL,

    #' @field max_budget_usd Maximum spend in USD for a query/session.
    max_budget_usd = NULL,

    #' @field fallback_model Fallback model identifier.
    fallback_model = NULL,

    #' @field add_dirs Additional directories made available to Claude.
    add_dirs = NULL,

    #' @field settings Custom settings file path.
    settings = NULL,

    #' @field setting_sources Character vector of setting sources.
    setting_sources = NULL,

    #' @field permission_prompt_tool_name Permission prompt tool name, commonly
    #'   `"stdio"` for control protocol integrations.
    permission_prompt_tool_name = NULL,

    #' @field plugins Character vector of plugin directories.
    plugins = NULL,

    #' @field env Named character vector/list of environment overrides.
    env = NULL,

    #' @field extra_args Named list/vector of extra CLI flags. `NULL` values are
    #'   emitted as boolean flags.
    extra_args = NULL,

    #' @field max_buffer_size Maximum stream JSON line size in bytes.
    max_buffer_size = 1024 * 1024,

    #' @field user Optional Unix user to execute as via sudo.
    user = NULL,

    #' @description Create a new `ClaudeOptions` object.
    #' @param model Model identifier string.
    #' @param system_prompt System prompt string.
    #' @param append_system_prompt Text to append to the system prompt.
    #' @param permission_mode One of `"default"`, `"acceptEdits"`, or
    #'   `"bypassPermissions"`.
    #' @param allowed_tools Character vector of allowed tools.
    #' @param disallowed_tools Character vector of disallowed tools.
    #' @param max_turns Integer maximum turns.
    #' @param max_tokens Integer maximum tokens per response.
    #' @param max_thinking_tokens Integer maximum thinking tokens.
    #' @param tools Character vector of base enabled tools.
    #' @param timeout Numeric seconds until subprocess timeout.
    #' @param working_dir Path to working directory.
    #' @param mcp_servers Named list of MCP server configurations.
    #' @param resume_session_id Existing Claude session id to resume.
    #' @param continue_conversation Whether to pass `--continue`.
    #' @param fork_session Whether to pass `--fork-session`.
    #' @param include_partial_messages Whether to include partial messages.
    #' @param agents JSON string describing subagents.
    #' @param json_schema Named list JSON schema for structured output.
    #' @param max_budget_usd Numeric maximum budget in USD.
    #' @param fallback_model Fallback model identifier.
    #' @param add_dirs Character vector of additional directories.
    #' @param settings Custom settings file path.
    #' @param setting_sources Character vector of setting sources.
    #' @param permission_prompt_tool_name Permission prompt tool name.
    #' @param plugins Character vector of plugin directories.
    #' @param env Named environment overrides for the subprocess.
    #' @param extra_args Named list/vector of extra CLI flags.
    #' @param max_buffer_size Maximum accepted stream JSON line size in bytes.
    #' @param user Optional Unix user to execute via `sudo -u`.
    initialize = function(
      model = NULL,
      system_prompt = NULL,
      append_system_prompt = NULL,
      permission_mode = "default",
      allowed_tools = NULL,
      disallowed_tools = NULL,
      max_turns = NULL,
      max_tokens = NULL,
      max_thinking_tokens = NULL,
      tools = NULL,
      timeout = 300,
      working_dir = NULL,
      mcp_servers = NULL,
      resume_session_id = NULL,
      continue_conversation = FALSE,
      fork_session = FALSE,
      include_partial_messages = FALSE,
      agents = NULL,
      json_schema = NULL,
      max_budget_usd = NULL,
      fallback_model = NULL,
      add_dirs = NULL,
      settings = NULL,
      setting_sources = NULL,
      permission_prompt_tool_name = NULL,
      plugins = NULL,
      env = NULL,
      extra_args = NULL,
      max_buffer_size = 1024 * 1024,
      user = NULL
    ) {
      valid_modes <- c("default", "acceptEdits", "bypassPermissions")
      if (!permission_mode %in% valid_modes) {
        cli::cli_abort(c(
          "{.arg permission_mode} must be one of {.val {valid_modes}}.",
          "x" = "Got: {.val {permission_mode}}"
        ))
      }
      if (!is.null(working_dir) && !dir.exists(working_dir)) {
        cli::cli_warn(
          "Working directory {.path {working_dir}} does not exist."
        )
      }
      if (!is.null(timeout) && (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0)) {
        cli::cli_abort("{.arg timeout} must be a positive number of seconds.")
      }
      if (!is.null(max_buffer_size) &&
          (!is.numeric(max_buffer_size) || length(max_buffer_size) != 1 || max_buffer_size <= 0)) {
        cli::cli_abort("{.arg max_buffer_size} must be a positive byte count.")
      }

      self$model                <- model
      self$system_prompt        <- system_prompt
      self$append_system_prompt <- append_system_prompt
      self$permission_mode      <- permission_mode
      self$allowed_tools        <- allowed_tools
      self$disallowed_tools     <- disallowed_tools
      self$max_turns            <- max_turns
      self$max_tokens           <- max_tokens
      self$max_thinking_tokens  <- max_thinking_tokens
      self$tools                <- tools
      self$timeout              <- timeout
      self$working_dir          <- working_dir %||% getwd()
      self$mcp_servers          <- mcp_servers
      self$resume_session_id    <- resume_session_id
      self$continue_conversation <- continue_conversation
      self$fork_session          <- fork_session
      self$include_partial_messages <- include_partial_messages
      self$agents               <- agents
      self$json_schema          <- json_schema
      self$max_budget_usd       <- max_budget_usd
      self$fallback_model       <- fallback_model
      self$add_dirs             <- add_dirs
      self$settings             <- settings
      self$setting_sources      <- setting_sources
      self$permission_prompt_tool_name <- permission_prompt_tool_name
      self$plugins              <- plugins
      self$env                  <- env
      self$extra_args           <- extra_args
      self$max_buffer_size      <- max_buffer_size
      self$user                 <- user
    },

    #' @description Build the CLI argument vector from these options.
    #' @param bidirectional Whether to build args for live stdin/stdout
    #'   `stream-json` mode instead of one-shot `--print` mode.
    #' @return Character vector of CLI flags.
    to_cli_args = function(bidirectional = FALSE) {
      args <- c("--output-format", "stream-json", "--verbose")
      if (isTRUE(bidirectional)) {
        args <- c(args, "--input-format", "stream-json")
      } else {
        args <- c(args, "--print")
      }

      if (!is.null(self$model)) {
        args <- c(args, "--model", self$model)
      }
      if (!is.null(self$system_prompt)) {
        args <- c(args, "--system-prompt", self$system_prompt)
      }
      if (!is.null(self$append_system_prompt)) {
        args <- c(args, "--append-system-prompt", self$append_system_prompt)
      }
      if (self$permission_mode == "acceptEdits") {
        args <- c(args, "--permission-mode", "acceptEdits")
      } else if (self$permission_mode == "bypassPermissions") {
        args <- c(args, "--dangerously-skip-permissions")
      } else if (self$permission_mode == "default") {
        args <- c(args, "--permission-mode", "default")
      }
      if (!is.null(self$tools)) {
        args <- c(args, "--tools", paste(self$tools, collapse = ","))
      }
      if (!is.null(self$allowed_tools)) {
        args <- c(args, "--allowedTools", paste(self$allowed_tools, collapse = ","))
      }
      if (!is.null(self$disallowed_tools)) {
        args <- c(args, "--disallowedTools", paste(self$disallowed_tools, collapse = ","))
      }
      if (!is.null(self$max_turns)) {
        args <- c(args, "--max-turns", as.character(self$max_turns))
      }
      if (!is.null(self$max_tokens)) {
        args <- c(args, "--max-tokens", as.character(self$max_tokens))
      }
      if (!is.null(self$max_thinking_tokens)) {
        args <- c(args, "--max-thinking-tokens", as.character(self$max_thinking_tokens))
      }
      if (!is.null(self$max_budget_usd)) {
        args <- c(args, "--max-budget-usd", as.character(self$max_budget_usd))
      }
      if (!is.null(self$fallback_model)) {
        args <- c(args, "--fallback-model", self$fallback_model)
      }
      if (!is.null(self$resume_session_id) && nzchar(self$resume_session_id)) {
        args <- c(args, "--resume", self$resume_session_id)
      }
      if (isTRUE(self$continue_conversation)) {
        args <- c(args, "--continue")
      }
      if (isTRUE(self$fork_session)) {
        args <- c(args, "--fork-session")
      }
      if (isTRUE(self$include_partial_messages)) {
        args <- c(args, "--include-partial-messages")
      }
      if (!is.null(self$agents)) {
        args <- c(args, "--agents", self$agents)
      }
      if (!is.null(self$json_schema)) {
        args <- c(args, "--json-schema", jsonlite::toJSON(self$json_schema, auto_unbox = TRUE))
      }
      for (dir in self$add_dirs %||% character(0)) {
        args <- c(args, "--add-dir", as.character(dir))
      }
      if (!is.null(self$settings)) {
        args <- c(args, "--settings", self$settings)
      }
      if (!is.null(self$setting_sources)) {
        args <- c(args, "--setting-sources", paste(self$setting_sources, collapse = ","))
      }
      if (!is.null(self$permission_prompt_tool_name)) {
        args <- c(args, "--permission-prompt-tool", self$permission_prompt_tool_name)
      }
      for (plugin in self$plugins %||% character(0)) {
        args <- c(args, "--plugin-dir", as.character(plugin))
      }
      if (!is.null(self$extra_args)) {
        for (name in names(self$extra_args)) {
          value <- self$extra_args[[name]]
          args <- c(args, paste0("--", name))
          if (!is.null(value)) {
            args <- c(args, as.character(value))
          }
        }
      }

      args
    }
  )
)

# Null-coalescing helper (internal)
`%||%` <- function(x, y) if (is.null(x)) y else x

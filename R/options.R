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

    #' @field timeout Duration in seconds before subprocess is killed.
    timeout = 300,

    #' @field working_dir Working directory for the CLI subprocess.
    working_dir = NULL,

    #' @field mcp_servers Named list of MCP server configurations.
    #'   Each element should be a list with `command`, `args`, and optional
    #'   `env` fields, keyed by server name.
    mcp_servers = NULL,

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
    #' @param timeout Numeric seconds until subprocess timeout.
    #' @param working_dir Path to working directory.
    #' @param mcp_servers Named list of MCP server configurations.
    initialize = function(
      model = NULL,
      system_prompt = NULL,
      append_system_prompt = NULL,
      permission_mode = "default",
      allowed_tools = NULL,
      disallowed_tools = NULL,
      max_turns = NULL,
      max_tokens = NULL,
      timeout = 300,
      working_dir = NULL,
      mcp_servers = NULL
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

      self$model                <- model
      self$system_prompt        <- system_prompt
      self$append_system_prompt <- append_system_prompt
      self$permission_mode      <- permission_mode
      self$allowed_tools        <- allowed_tools
      self$disallowed_tools     <- disallowed_tools
      self$max_turns            <- max_turns
      self$max_tokens           <- max_tokens
      self$timeout              <- timeout
      self$working_dir          <- working_dir %||% getwd()
      self$mcp_servers          <- mcp_servers
    },

    #' @description Build the CLI argument vector from these options.
    #' @return Character vector of CLI flags.
    to_cli_args = function() {
      args <- c("--output-format", "stream-json", "--print", "--no-color")

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
        args <- c(args, "--accept-edits")
      } else if (self$permission_mode == "bypassPermissions") {
        args <- c(args, "--dangerously-skip-permissions")
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

      args
    }
  )
)

# Null-coalescing helper (internal)
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Start a Claude CLI subprocess
#'
#' @description
#' Spawns `claude` as a subprocess using `processx`, with bidirectional
#' stdin/stdout pipes for streaming JSON-LD communication.
#'
#' @param prompt Initial prompt string.
#' @param options A `ClaudeOptions` object.
#' @return A `processx::process` object.
#' @keywords internal
start_transport <- function(prompt, options) {
  if (is.null(prompt) || !nzchar(trimws(as.character(prompt)))) {
    cli::cli_abort("{.arg prompt} must be a non-empty string.")
  }

  cli_path <- find_claude_cli()
  args <- options$to_cli_args()

  # Write MCP server config to a temp file if provided
  if (!is.null(options$mcp_servers) && length(options$mcp_servers) > 0) {
    mcp_file <- tempfile(fileext = ".json")
    jsonlite::write_json(
      list(mcpServers = options$mcp_servers),
      mcp_file,
      auto_unbox = TRUE,
      pretty     = FALSE
    )
    args <- c(args, "--mcp-config", mcp_file)
    # tempfile() lives in tempdir() and is cleaned up at R session end
  }

  args <- c(args, prompt)

  proc <- processx::process$new(
    command = cli_path,
    args    = args,
    stdin   = "|",
    stdout  = "|",
    stderr  = "|",
    wd      = options$working_dir,
    cleanup = TRUE
  )

  proc
}

#' Send a follow-up message to a running Claude process
#'
#' @param proc A `processx::process` object.
#' @param prompt Character string prompt.
#' @keywords internal
send_message <- function(proc, prompt) {
  if (is.null(prompt) || !nzchar(trimws(as.character(prompt)))) {
    cli::cli_abort("{.arg prompt} must be a non-empty string.")
  }
  if (!proc$is_alive()) {
    cli::cli_abort("Claude process is no longer running.")
  }
  proc$write_input(paste0(prompt, "\n"))
}

#' Read and stream all messages until result
#'
#' Blocks, polling stdout until a `result` message is received or the
#' process exits. Dispatches pre- and post-tool-use hooks if a registry is
#' supplied.
#'
#' @param proc A `processx::process` object.
#' @param hooks A `HookRegistry` or `NULL`.
#' @param on_message Optional callback `function(msg)` called for each message.
#' @param timeout Numeric seconds before aborting with a timeout error.
#' @return A list with fields `messages` (all parsed msgs), `text` (final
#'   concatenated assistant text), and `result` (the result message or `NULL`).
#' @keywords internal
collect_messages <- function(proc, hooks = NULL, on_message = NULL, timeout = 300) {
  messages        <- list()
  text_parts      <- character(0)
  tool_use_inputs <- list()   # keyed by tool_use id for post-hook dispatch
  deadline        <- Sys.time() + timeout

  repeat {
    if (Sys.time() > deadline) {
      cli::cli_abort("Timed out waiting for Claude response.")
    }

    lines <- tryCatch(
      proc$read_output_lines(n = 100, timeout = 200),
      error = function(e) character(0)
    )

    for (line in lines) {
      msg <- parse_stream_line(line)
      if (is.null(msg)) next

      messages <- c(messages, list(msg))

      # Pre-hook dispatch: run before tool executes
      if (!is.null(hooks) && is_tool_use_message(msg)) {
        for (block in msg$message$content) {
          if (identical(block$type, "tool_use")) {
            result <- hooks$run_pre_hooks(block$name, block$input %||% list())
            if (identical(result$action, "block")) {
              cli::cli_alert_warning(
                "Hook blocked tool {.val {block$name}}: {result$reason}"
              )
            }
            # Track input by tool_use id so post-hook can match it
            if (!is.null(block$id)) {
              tool_use_inputs[[block$id]] <- list(
                name  = block$name,
                input = block$input %||% list()
              )
            }
          }
        }
      }

      # Post-hook dispatch: run after tool result arrives (user message)
      if (!is.null(hooks) && identical(msg$type, "user")) {
        for (block in (msg$message$content %||% list())) {
          if (identical(block$type, "tool_result") && !is.null(block$tool_use_id)) {
            tui <- tool_use_inputs[[block$tool_use_id]]
            if (!is.null(tui)) {
              hooks$run_post_hooks(tui$name, tui$input, block$content)
            }
          }
        }
      }

      # Collect assistant text
      if (identical(msg$type, "assistant")) {
        txt <- extract_text(msg)
        if (nzchar(txt)) text_parts <- c(text_parts, txt)
      }

      if (!is.null(on_message)) {
        tryCatch(
          on_message(msg),
          error = function(e) {
            cli::cli_warn("on_message callback error: {conditionMessage(e)}")
          }
        )
      }

      if (is_result_message(msg)) {
        return(list(
          messages = messages,
          text     = paste(text_parts, collapse = ""),
          result   = msg
        ))
      }
    }

    # Process exited without sending a result message
    if (!proc$is_alive() && length(lines) == 0) {
      stderr_out <- proc$read_all_error()
      if (nzchar(stderr_out)) {
        cli::cli_abort(c(
          "Claude process exited unexpectedly.",
          "x" = stderr_out
        ))
      }
      break
    }
  }

  list(messages = messages, text = paste(text_parts, collapse = ""), result = NULL)
}

#' Locate the `claude` CLI executable
#'
#' Checks `CLAUDE_CLI_PATH` env var first, then `PATH`.
#'
#' @return Absolute path string.
#' @keywords internal
find_claude_cli <- function() {
  env_path <- Sys.getenv("CLAUDE_CLI_PATH", unset = "")
  if (nzchar(env_path) && file.exists(env_path)) return(env_path)

  found <- Sys.which("claude")
  if (!nzchar(found)) {
    cli::cli_abort(c(
      "Claude Code CLI not found.",
      "i" = "Install it with: {.code npm install -g @anthropic-ai/claude-code}",
      "i" = "Or set {.envvar CLAUDE_CLI_PATH} to its location."
    ))
  }
  found
}

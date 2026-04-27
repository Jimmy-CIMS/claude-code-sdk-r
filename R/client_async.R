#' Asynchronous Claude Agent Client
#'
#' @description
#' `ClaudeAsyncClient` wraps Claude Code CLI calls in `promises::promise`,
#' using `later` for non-blocking event-loop polling. This mirrors
#' `ClaudeAsyncClient` from the Java SDK.
#'
#' Ideal for use inside **Shiny** apps or any context that already runs an
#' event loop (`later::run_now()` for scripts).
#'
#' @examples
#' \dontrun{
#' library(promises)
#'
#' client <- claude_async_client(model = "claude-sonnet-4-20250514")
#'
#' # Returns a promise resolving to the full result list
#' p <- client$connect_text("What is 2 + 2?")
#'
#' p %...>% cat()  # prints when resolved
#'
#' # Drive the event loop in a script context
#' later::run_now(timeout = 30)
#' }
#'
#' @export
ClaudeAsyncClient <- R6::R6Class(
  "ClaudeAsyncClient",

  public = list(

    #' @field last_messages All parsed messages from the most recent turn.
    last_messages = NULL,

    #' @field last_text Concatenated assistant text from the most recent turn.
    last_text = NULL,

    #' @field last_result The result/stop message from the most recent turn.
    last_result = NULL,

    #' @description Create a new `ClaudeAsyncClient`.
    #' @param options A `ClaudeOptions` object.
    #' @param hooks A `HookRegistry` or `NULL`.
    initialize = function(options = NULL, hooks = NULL) {
      private$options <- options %||% ClaudeOptions$new()
      private$hooks   <- hooks
    },

    #' @description
    #' Start the subprocess and return a `promise` that resolves to the full
    #' result list (`text`, `messages`, `result`).
    #' @param prompt Character string.
    #' @param on_message Optional `function(msg)` called per streamed message.
    #' @return A `promise`.
    connect = function(prompt, on_message = NULL) {
      if (private$connected) {
        return(promises::promise_reject(
          "Already connected. Use $query() for follow-up turns."
        ))
      }
      private$proc      <- start_transport(prompt, private$options)
      private$connected <- TRUE
      private$stream_promise(on_message)
    },

    #' @description
    #' Start and return a `promise` resolving to assistant text only.
    #' @param prompt Character string.
    #' @return A `promise<character>`.
    connect_text = function(prompt) {
      self$connect(prompt) %...>% (function(r) r$text)
    },

    #' @description
    #' Send a follow-up and return a promise resolving to the result list.
    #' @param prompt Character string.
    #' @param on_message Optional callback.
    #' @return A `promise`.
    query = function(prompt, on_message = NULL) {
      if (!private$connected) {
        return(promises::promise_reject("Not connected. Call $connect() first."))
      }
      send_message(private$proc, prompt)
      private$stream_promise(on_message)
    },

    #' @description
    #' Follow-up returning a promise of text only.
    #' @param prompt Character string.
    #' @return A `promise<character>`.
    query_text = function(prompt) {
      self$query(prompt) %...>% (function(r) r$text)
    },

    #' @description Terminate the subprocess and cancel any pending promise.
    close = function() {
      private$closed <- TRUE
      if (!is.null(private$proc) && private$proc$is_alive()) {
        private$proc$kill()
      }
      private$connected <- FALSE
      invisible(NULL)
    }
  ),

  private = list(
    proc      = NULL,
    options   = NULL,
    hooks     = NULL,
    connected = FALSE,
    closed    = FALSE,

    store_result = function(result) {
      self$last_messages <- result$messages
      self$last_text     <- result$text
      self$last_result   <- result$result
    },

    # Returns a promise that resolves when a result message arrives,
    # using later-based non-blocking polling.
    stream_promise = function(on_message) {
      proc            <- private$proc
      hooks           <- private$hooks
      messages        <- list()
      text_parts      <- character(0)
      tool_use_inputs <- list()
      deadline        <- Sys.time() + private$options$timeout

      # Capture self for use inside async callbacks
      self_ref <- self

      promises::promise(function(resolve, reject) {
        poll <- function() {
          # Abort cleanly if close() was called while polling
          if (self_ref$.__enclos_env__$private$closed) {
            reject("Client was closed while waiting for response.")
            return()
          }

          if (Sys.time() > deadline) {
            reject("Timed out waiting for Claude response.")
            return()
          }

          tryCatch({
            ready <- proc$poll_io(0)
            lines <- if (identical(ready[["output"]], "ready")) {
              proc$read_output_lines(n = 100)
            } else {
              character(0)
            }

            for (line in lines) {
              msg <- parse_stream_line(line)
              if (is.null(msg)) next

              messages <<- c(messages, list(msg))

              # Pre-hook dispatch
              if (!is.null(hooks) && is_tool_use_message(msg)) {
                for (block in msg$message$content) {
                  if (identical(block$type, "tool_use")) {
                    result <- hooks$run_pre_hooks(block$name, block$input %||% list())
                    if (identical(result$action, "block")) {
                      cli::cli_alert_warning(
                        "Hook blocked tool {.val {block$name}}: {result$reason}"
                      )
                    }
                    if (!is.null(block$id)) {
                      tool_use_inputs[[block$id]] <<- list(
                        name  = block$name,
                        input = block$input %||% list()
                      )
                    }
                  }
                }
              }

              # Post-hook dispatch
              if (!is.null(hooks) && identical(msg$type, "user")) {
                for (block in (msg$message$content %||% list())) {
                  if (identical(block$type, "tool_result") &&
                      !is.null(block$tool_use_id)) {
                    tui <- tool_use_inputs[[block$tool_use_id]]
                    if (!is.null(tui)) {
                      hooks$run_post_hooks(tui$name, tui$input, block$content)
                    }
                  }
                }
              }

              if (identical(msg$type, "assistant")) {
                txt <- extract_text(msg)
                if (nzchar(txt)) text_parts <<- c(text_parts, txt)
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
                r <- list(
                  messages = messages,
                  text     = paste(text_parts, collapse = ""),
                  result   = msg
                )
                self_ref$.__enclos_env__$private$store_result(r)
                resolve(r)
                return()
              }
            }

            if (!proc$is_alive() && length(lines) == 0) {
              stderr_out <- proc$read_all_error()
              if (nzchar(stderr_out)) {
                reject(paste("Claude process exited:", stderr_out))
              } else {
                r <- list(
                  messages = messages,
                  text     = paste(text_parts, collapse = ""),
                  result   = NULL
                )
                self_ref$.__enclos_env__$private$store_result(r)
                resolve(r)
              }
              return()
            }

            # Reschedule poll on the event loop (non-blocking)
            later::later(poll, delay = 0.05)

          }, error = function(e) reject(conditionMessage(e)))
        }

        later::later(poll, delay = 0)
      })
    }
  ),

  cloneable = FALSE
)

#' Create an asynchronous Claude client
#'
#' @description Convenience constructor for `ClaudeAsyncClient`.
#'
#' @param model Model identifier string.
#' @param system_prompt System prompt override.
#' @param permission_mode One of `"default"`, `"acceptEdits"`,
#'   `"bypassPermissions"`.
#' @param allowed_tools Character vector of allowed tool names.
#' @param disallowed_tools Character vector of disallowed tool names.
#' @param max_turns Integer maximum agent turns.
#' @param max_tokens Integer maximum tokens per response.
#' @param timeout Numeric seconds before subprocess timeout.
#' @param working_dir Path to working directory.
#' @param mcp_servers Named list of MCP server configurations.
#' @param hooks A `HookRegistry` object or `NULL`.
#' @return A `ClaudeAsyncClient` object.
#'
#' @examples
#' \dontrun{
#' client <- claude_async_client(model = "claude-sonnet-4-20250514")
#' p <- client$connect_text("Explain promises in R.")
#' p %...>% cat()
#' later::run_now(timeout = 60)
#' }
#'
#' @export
claude_async_client <- function(
  model            = NULL,
  system_prompt    = NULL,
  permission_mode  = "default",
  allowed_tools    = NULL,
  disallowed_tools = NULL,
  max_turns        = NULL,
  max_tokens       = NULL,
  timeout          = 300,
  working_dir      = NULL,
  mcp_servers      = NULL,
  hooks            = NULL
) {
  opts <- ClaudeOptions$new(
    model            = model,
    system_prompt    = system_prompt,
    permission_mode  = permission_mode,
    allowed_tools    = allowed_tools,
    disallowed_tools = disallowed_tools,
    max_turns        = max_turns,
    max_tokens       = max_tokens,
    timeout          = timeout,
    working_dir      = working_dir,
    mcp_servers      = mcp_servers
  )
  ClaudeAsyncClient$new(options = opts, hooks = hooks)
}

#' Asynchronous Claude Agent Client
#'
#' @description
#' `ClaudeAsyncClient` wraps Claude Code CLI calls in `promises::promise`,
#' using `later` for non-blocking event-loop polling.
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
    #' @param tool_permission_callback Optional callback for Claude CLI
    #'   `can_use_tool` control requests. It may return `permission_allow()`,
    #'   `permission_deny()`, or a promise resolving to either.
    initialize = function(options = NULL, hooks = NULL, tool_permission_callback = NULL) {
      private$options <- options %||% ClaudeOptions$new()
      private$hooks   <- hooks
      private$tool_permission_callback <- tool_permission_callback
      if (!is.null(tool_permission_callback) &&
          is.null(private$options$permission_prompt_tool_name)) {
        private$options$permission_prompt_tool_name <- "stdio"
      }
    },

    #' @description
    #' Start a Claude session and return a `promise` that resolves to the full
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
      promises::`%...>%`(
        private$run_turn(prompt, on_message, resume = FALSE),
        (function(r) {
          private$connected <- TRUE
          r
        })
      )
    },

    #' @description
    #' Start and return a `promise` resolving to assistant text only.
    #' @param prompt Character string.
    #' @return A `promise<character>`.
    connect_text = function(prompt) {
      promises::`%...>%`(self$connect(prompt), (function(r) r$text))
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
      private$run_turn(prompt, on_message, resume = TRUE)
    },

    #' @description
    #' Follow-up returning a promise of text only.
    #' @param prompt Character string.
    #' @return A `promise<character>`.
    query_text = function(prompt) {
      promises::`%...>%`(self$query(prompt), (function(r) r$text))
    },

    #' @description Forget the stored Claude session and cancel any pending promise.
    close = function() {
      private$closed <- TRUE
      if (!is.null(private$proc) && private$proc$is_alive()) {
        private$proc$kill()
      }
      private$proc <- NULL
      private$connected <- FALSE
      private$session_id <- NULL
      private$live_started <- FALSE
      invisible(NULL)
    }
  ),

  private = list(
    proc      = NULL,
    options   = NULL,
    hooks     = NULL,
    tool_permission_callback = NULL,
    connected = FALSE,
    closed    = FALSE,
    session_id = NULL,
    live_started = FALSE,

    store_result = function(result) {
      self$last_messages <- result$messages
      self$last_text     <- result$text
      self$last_result   <- result$result
      private$session_id <- extract_session_id(result)
    },

    run_turn = function(prompt, on_message, resume) {
      private$closed <- FALSE
      if (!is.null(private$tool_permission_callback)) {
        if (is.null(private$proc) || !private$proc$is_alive()) {
          private$proc <- start_bidirectional_transport(private$options)
          private$live_started <- TRUE
        }
        send_live_user_message(
          private$proc,
          prompt,
          if (resume) private$session_id %||% "default" else "default"
        )
        return(private$stream_promise(on_message, keep_process = TRUE))
      }

      if (resume) {
        private$options$resume_session_id <- private$session_id
      } else {
        private$options$resume_session_id <- NULL
      }
      private$proc <- start_transport(prompt, private$options)
      private$stream_promise(on_message, keep_process = FALSE)
    },

    # Returns a promise that resolves when a result message arrives,
    # using later-based non-blocking polling.
    stream_promise = function(on_message, keep_process = FALSE) {
      proc            <- private$proc
      hooks           <- private$hooks
      permission_callback <- private$tool_permission_callback
      messages        <- list()
      text_parts      <- character(0)
      tool_use_inputs <- list()
      deadline        <- Sys.time() + private$options$timeout
      waiting_control <- FALSE

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
            if (waiting_control) {
              later::later(poll, delay = 0.05)
              return()
            }

            ready <- proc$poll_io(0)
            lines <- if (identical(ready[["output"]], "ready")) {
              proc$read_output_lines(n = 100)
            } else {
              character(0)
            }

            for (line in lines) {
              msg <- parse_stream_line(
                line,
                max_buffer_size = self_ref$.__enclos_env__$private$options$max_buffer_size
              )
              if (is.null(msg)) next

              if (identical(msg$type, "control_request")) {
                request_id <- msg$request_id
                request <- msg$request %||% list()
                subtype <- request$subtype %||% ""

                if (identical(subtype, "can_use_tool")) {
                  context <- list(
                    request_id = request_id,
                    permission_suggestions = request$permission_suggestions %||% list(),
                    blocked_path = request$blocked_path %||% NULL
                  )
                  callback_result <- tryCatch({
                    if (is.null(permission_callback)) {
                      permission_allow()
                    } else {
                      permission_callback(
                        request$tool_name %||% "",
                        request$input %||% list(),
                        context
                      )
                    }
                  }, error = function(e) e)

                  if (inherits(callback_result, "condition")) {
                    send_control_response(proc, request_id, error = conditionMessage(callback_result))
                  } else if (promises::is.promise(callback_result)) {
                    waiting_control <<- TRUE
                    promises::`%...>%`(
                      callback_result,
                      (function(value) {
                        send_control_response(
                          proc,
                          request_id,
                          normalize_permission_result(value)
                        )
                        waiting_control <<- FALSE
                        later::later(poll, delay = 0)
                      })
                    ) %...!% (function(err) {
                      message <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
                      send_control_response(proc, request_id, error = message)
                      waiting_control <<- FALSE
                      later::later(poll, delay = 0)
                    })
                    return()
                  } else {
                    send_control_response(
                      proc,
                      request_id,
                      normalize_permission_result(callback_result)
                    )
                  }
                } else if (identical(subtype, "initialize")) {
                  send_control_response(proc, request_id, list(status = "ok"))
                } else if (identical(subtype, "hook_callback")) {
                  send_control_response(proc, request_id, list(continue = TRUE))
                } else {
                  send_control_response(proc, request_id, list())
                }
                next
              }

              if (identical(msg$type, "control_response")) {
                next
              }

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
                if (!keep_process) {
                  self_ref$.__enclos_env__$private$proc <- NULL
                }
                resolve(r)
                return()
              }
            }

            if (!proc$is_alive() && length(lines) == 0) {
              stderr_out <- proc$read_all_error()
              if (nzchar(stderr_out)) {
                self_ref$.__enclos_env__$private$proc <- NULL
                reject(paste("Claude process exited:", stderr_out))
              } else {
                self_ref$.__enclos_env__$private$proc <- NULL
                reject("Claude process exited without emitting a result message.")
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
#' @param tool_permission_callback Optional callback for Claude CLI
#'   `can_use_tool` requests. May return `permission_allow()`,
#'   `permission_deny()`, or a promise resolving to either.
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
  tool_permission_callback = NULL,
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
  ClaudeAsyncClient$new(
    options = opts,
    hooks = hooks,
    tool_permission_callback = tool_permission_callback
  )
}

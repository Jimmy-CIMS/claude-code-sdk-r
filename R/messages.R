#' Parse a single JSON line from the Claude CLI stream
#'
#' @param line A single character string (one line of stream-json output).
#' @param max_buffer_size Maximum accepted line size in bytes.
#' @return A named list with at minimum a `type` field, plus type-specific
#'   fields. Returns `NULL` if the line is empty or cannot be parsed.
#' @keywords internal
parse_stream_line <- function(line, max_buffer_size = 1024 * 1024) {
  line <- trimws(line)
  if (!nzchar(line)) return(NULL)
  if (nchar(line, type = "bytes") > max_buffer_size) {
    cli::cli_warn(
      "Skipping stream line larger than max_buffer_size ({max_buffer_size} bytes)."
    )
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_warn("Failed to parse stream line: {line}")
      NULL
    }
  )
}

#' Extract the text content from an AssistantMessage
#'
#' @param msg A parsed message list (type == "assistant").
#' @return Character string of concatenated text blocks, or `""`.
#' @keywords internal
extract_text <- function(msg) {
  if (is.null(msg$message) || is.null(msg$message$content)) return("")

  blocks <- msg$message$content
  texts <- vapply(blocks, function(block) {
    if (identical(block$type, "text")) block$text %||% "" else ""
  }, character(1))

  paste(texts[nzchar(texts)], collapse = "")
}

#' Check whether a parsed message signals end of turn
#'
#' @param msg Parsed message list.
#' @return `TRUE` if this is a result/stop message.
#' @keywords internal
is_result_message <- function(msg) {
  identical(msg$type, "result")
}

#' Extract Claude session id from a completed turn
#'
#' @param result Completed turn result list with `messages` and `result`.
#' @return Character session id or `NULL`.
#' @keywords internal
extract_session_id <- function(result) {
  from_result <- result$result$session_id %||% NULL
  if (!is.null(from_result) && nzchar(from_result)) {
    return(from_result)
  }

  for (msg in (result$messages %||% list())) {
    if (identical(msg$type, "system") &&
        identical(msg$subtype, "init") &&
        !is.null(msg$session_id) &&
        nzchar(msg$session_id)) {
      return(msg$session_id)
    }
  }

  NULL
}

#' Check whether a parsed message is a tool-use block
#'
#' @param msg Parsed message list.
#' @return `TRUE` if the message contains tool_use content blocks.
#' @keywords internal
is_tool_use_message <- function(msg) {
  if (!identical(msg$type, "assistant")) return(FALSE)
  if (is.null(msg$message)) return(FALSE)
  blocks <- msg$message$content %||% list()
  any(vapply(blocks, function(b) identical(b$type, "tool_use"), logical(1)))
}

#' Pretty-print a parsed message (for interactive use)
#'
#' @param msg Parsed message list.
#' @return Invisibly returns `msg`.
#' @export
print_message <- function(msg) {
  if (is.null(msg) || is.null(msg$type)) {
    cli::cli_alert_warning("[unknown] malformed message")
    return(invisible(msg))
  }

  switch(
    msg$type,
    system    = cli::cli_alert_info("[system] {msg$system}"),
    assistant = {
      txt <- extract_text(msg)
      if (nzchar(txt)) cli::cli_text("{txt}")
      if (is_tool_use_message(msg)) {
        for (block in msg$message$content) {
          if (identical(block$type, "tool_use")) {
            cli::cli_alert_success("[tool_use] {block$name}")
          }
        }
      }
    },
    result    = {
      cost <- msg$total_cost_usd %||% msg$cost_usd %||% 0
      cli::cli_alert_info(
        "[result] subtype={msg$subtype} cost=${round(cost, 6)}"
      )
    },
    cli::cli_alert_warning("[{msg$type}] (unhandled)")
  )
  invisible(msg)
}

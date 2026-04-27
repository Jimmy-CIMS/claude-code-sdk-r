#' Allow a Claude tool permission request
#'
#' @param updated_input Optional named list with modified tool input.
#' @return A permission result consumed by `tool_permission_callback`.
#' @export
permission_allow <- function(updated_input = NULL) {
  list(behavior = "allow", updatedInput = updated_input)
}

#' Deny a Claude tool permission request
#'
#' @param message Optional denial message sent back to Claude.
#' @return A permission result consumed by `tool_permission_callback`.
#' @export
permission_deny <- function(message = "Denied by user") {
  list(behavior = "deny", message = message)
}

normalize_permission_result <- function(result) {
  result <- result %||% permission_allow()
  if (isTRUE(result$allow)) {
    result$behavior <- "allow"
  }
  if (isTRUE(result$deny)) {
    result$behavior <- "deny"
  }
  behavior <- result$behavior %||% result$action %||% "allow"
  if (identical(behavior, "block")) behavior <- "deny"

  response <- list(behavior = behavior)
  if (identical(behavior, "allow") && !is.null(result$updatedInput)) {
    response$updatedInput <- result$updatedInput
  }
  if (identical(behavior, "allow") && !is.null(result$updated_input)) {
    response$updatedInput <- result$updated_input
  }
  if (identical(behavior, "deny") && !is.null(result$message)) {
    response$message <- result$message
  }
  if (identical(behavior, "deny") && !is.null(result$reason)) {
    response$message <- result$reason
  }
  response
}

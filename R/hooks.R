#' Create a Hook Registry
#'
#' @description
#' A `HookRegistry` stores pre/post tool-use callbacks. Register functions that
#' receive tool input and return either `hook_allow()` or `hook_block(reason)`.
#'
#' Pre-hooks run before a tool executes and can block it. Post-hooks run after
#' a tool executes (receiving both input and output) and are informational only.
#'
#' @examples
#' hooks <- HookRegistry$new()
#'
#' # Block dangerous shell commands
#' hooks$register_pre_tool_use("Bash", function(input) {
#'   if (grepl("rm -rf", input$command %||% "")) {
#'     hook_block("Dangerous command blocked")
#'   } else {
#'     hook_allow()
#'   }
#' })
#'
#' # Log all file writes
#' hooks$register_post_tool_use("Write", function(ctx) {
#'   message("File written: ", ctx$input$file_path)
#' })
#'
#' @export
HookRegistry <- R6::R6Class(
  "HookRegistry",
  private = list(
    pre_hooks  = list(),
    post_hooks = list()
  ),
  public = list(

    #' @description Register a pre-tool-use hook for a specific tool.
    #' @param tool_name Name of the tool (e.g. `"Bash"`, `"Write"`).
    #' @param callback A function receiving a named list of tool inputs,
    #'   returning `hook_allow()` or `hook_block(reason)`.
    register_pre_tool_use = function(tool_name, callback) {
      if (!is.function(callback)) {
        cli::cli_abort("{.arg callback} must be a function.")
      }
      private$pre_hooks[[tool_name]] <- c(
        private$pre_hooks[[tool_name]],
        list(callback)
      )
      invisible(self)
    },

    #' @description Register a post-tool-use hook for a specific tool.
    #' @param tool_name Name of the tool.
    #' @param callback A function receiving a named list with `input` and
    #'   `output` fields. Return value is ignored.
    register_post_tool_use = function(tool_name, callback) {
      if (!is.function(callback)) {
        cli::cli_abort("{.arg callback} must be a function.")
      }
      private$post_hooks[[tool_name]] <- c(
        private$post_hooks[[tool_name]],
        list(callback)
      )
      invisible(self)
    },

    #' @description Run all pre-tool-use hooks for a tool.
    #' @param tool_name Tool name string.
    #' @param input Named list of tool input arguments.
    #' @return A `hook_allow()` or `hook_block()` result. Errors in individual
    #'   callbacks are caught and logged; a failing callback does not block.
    run_pre_hooks = function(tool_name, input) {
      callbacks <- private$pre_hooks[[tool_name]] %||% list()
      for (cb in callbacks) {
        result <- tryCatch(
          cb(input),
          error = function(e) {
            cli::cli_warn(
              "Pre-hook callback error for {.val {tool_name}}: {conditionMessage(e)}"
            )
            hook_allow()
          }
        )
        if (identical(result$action, "block")) return(result)
      }
      hook_allow()
    },

    #' @description Run all post-tool-use hooks for a tool.
    #' @param tool_name Tool name string.
    #' @param input Named list of tool input arguments.
    #' @param output Tool output/result (may be `NULL`).
    #' @return Invisibly `NULL`. Errors in individual callbacks are caught
    #'   and logged.
    run_post_hooks = function(tool_name, input, output) {
      callbacks <- private$post_hooks[[tool_name]] %||% list()
      for (cb in callbacks) {
        tryCatch(
          cb(list(input = input, output = output)),
          error = function(e) {
            cli::cli_warn(
              "Post-hook callback error for {.val {tool_name}}: {conditionMessage(e)}"
            )
          }
        )
      }
      invisible(NULL)
    }
  )
)

#' Signal that a hook allows tool execution to proceed
#' @return A list with `action = "allow"`.
#' @export
hook_allow <- function() list(action = "allow")

#' Signal that a hook blocks tool execution
#' @param reason Human-readable reason string shown to Claude.
#' @return A list with `action = "block"` and `reason`.
#' @export
hook_block <- function(reason = "Blocked by hook") {
  list(action = "block", reason = reason)
}

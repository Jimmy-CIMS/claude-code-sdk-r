library(testthat)
library(claudeAgentR)

# ---------------------------------------------------------------------------
# ClaudeOptions — field storage
# ---------------------------------------------------------------------------

test_that("ClaudeOptions stores all fields correctly", {
  opts <- ClaudeOptions$new(
    model            = "claude-sonnet-4-20250514",
    system_prompt    = "Be concise",
    max_turns        = 5,
    max_tokens       = 1024,
    timeout          = 60,
    permission_mode  = "default",
    allowed_tools    = c("Read", "Bash"),
    disallowed_tools = c("Write"),
    resume_session_id = "session-123"
  )
  expect_equal(opts$model,            "claude-sonnet-4-20250514")
  expect_equal(opts$system_prompt,    "Be concise")
  expect_equal(opts$max_turns,        5)
  expect_equal(opts$max_tokens,       1024)
  expect_equal(opts$timeout,          60)
  expect_equal(opts$allowed_tools,    c("Read", "Bash"))
  expect_equal(opts$disallowed_tools, c("Write"))
  expect_equal(opts$resume_session_id, "session-123")
})

test_that("ClaudeOptions defaults working_dir to getwd()", {
  opts <- ClaudeOptions$new()
  expect_equal(opts$working_dir, getwd())
})

test_that("ClaudeOptions accepts explicit working_dir", {
  opts <- ClaudeOptions$new(working_dir = tempdir())
  expect_equal(opts$working_dir, tempdir())
})

test_that("ClaudeOptions warns when working_dir does not exist", {
  expect_warning(
    ClaudeOptions$new(working_dir = "/nonexistent/path/xyz"),
    "does not exist"
  )
})

test_that("ClaudeOptions rejects invalid permission_mode", {
  expect_error(
    ClaudeOptions$new(permission_mode = "badMode"),
    "permission_mode"
  )
})

test_that("ClaudeOptions stores mcp_servers", {
  servers <- list(
    myserver = list(command = "node", args = list("server.js"))
  )
  opts <- ClaudeOptions$new(mcp_servers = servers)
  expect_equal(opts$mcp_servers, servers)
})

# ---------------------------------------------------------------------------
# ClaudeOptions — to_cli_args()
# ---------------------------------------------------------------------------

test_that("to_cli_args includes required base flags", {
  args <- ClaudeOptions$new()$to_cli_args()
  expect_true("--output-format" %in% args)
  expect_true("stream-json"     %in% args)
  expect_true("--verbose"       %in% args)
  expect_true("--print"         %in% args)
})

test_that("to_cli_args supports bidirectional stream-json mode", {
  args <- ClaudeOptions$new(permission_prompt_tool_name = "stdio")$to_cli_args(bidirectional = TRUE)
  expect_true("--input-format" %in% args)
  expect_true("--output-format" %in% args)
  expect_true("--permission-prompt-tool" %in% args)
  expect_true("stdio" %in% args)
  expect_false("--print" %in% args)
})

test_that("to_cli_args includes --resume when session id is set", {
  args <- ClaudeOptions$new(resume_session_id = "session-123")$to_cli_args()
  idx <- which(args == "--resume")
  expect_length(idx, 1)
  expect_equal(args[idx + 1], "session-123")
})

test_that("to_cli_args includes --model when set", {
  args <- ClaudeOptions$new(model = "claude-sonnet-4-20250514")$to_cli_args()
  expect_true("--model" %in% args)
  expect_true("claude-sonnet-4-20250514" %in% args)
})

test_that("to_cli_args includes --max-turns when set", {
  args <- ClaudeOptions$new(max_turns = 3)$to_cli_args()
  expect_true("--max-turns" %in% args)
  expect_true("3" %in% args)
})

test_that("to_cli_args includes --max-tokens when set", {
  args <- ClaudeOptions$new(max_tokens = 512)$to_cli_args()
  expect_true("--max-tokens" %in% args)
  expect_true("512" %in% args)
})

test_that("to_cli_args omits --max-tokens when NULL", {
  args <- ClaudeOptions$new()$to_cli_args()
  expect_false("--max-tokens" %in% args)
})

test_that("to_cli_args includes --system-prompt when set", {
  args <- ClaudeOptions$new(system_prompt = "Be helpful")$to_cli_args()
  expect_true("--system-prompt" %in% args)
  expect_true("Be helpful" %in% args)
})

test_that("to_cli_args includes --allowedTools when set", {
  args <- ClaudeOptions$new(allowed_tools = c("Read", "Bash"))$to_cli_args()
  expect_true("--allowedTools" %in% args)
  idx <- which(args == "--allowedTools")
  expect_equal(args[idx + 1], "Read,Bash")
})

test_that("to_cli_args includes --disallowedTools when set", {
  args <- ClaudeOptions$new(disallowed_tools = c("Write"))$to_cli_args()
  expect_true("--disallowedTools" %in% args)
})

test_that("to_cli_args adds --permission-mode for acceptEdits mode", {
  args <- ClaudeOptions$new(permission_mode = "acceptEdits")$to_cli_args()
  idx <- which(args == "--permission-mode")
  expect_length(idx, 1)
  expect_equal(args[idx + 1], "acceptEdits")
  expect_false("--dangerously-skip-permissions" %in% args)
})

test_that("to_cli_args adds --dangerously-skip-permissions for bypassPermissions", {
  args <- ClaudeOptions$new(permission_mode = "bypassPermissions")$to_cli_args()
  expect_true("--dangerously-skip-permissions" %in% args)
  expect_false("--accept-edits" %in% args)
})

test_that("to_cli_args adds explicit --permission-mode for default mode", {
  args <- ClaudeOptions$new(permission_mode = "default")$to_cli_args()
  idx <- which(args == "--permission-mode")
  expect_length(idx, 1)
  expect_equal(args[idx + 1], "default")
  expect_false("--dangerously-skip-permissions" %in% args)
})

test_that("to_cli_args includes advanced Java-parity flags", {
  args <- ClaudeOptions$new(
    tools = c("Read", "Write"),
    max_thinking_tokens = 2048,
    max_budget_usd = 1.5,
    fallback_model = "claude-haiku-4-5-20251001",
    append_system_prompt = "Extra rules",
    continue_conversation = TRUE,
    fork_session = TRUE,
    include_partial_messages = TRUE,
    agents = "{\"researcher\":{}}",
    json_schema = list(type = "object"),
    add_dirs = c("/tmp/a", "/tmp/b"),
    settings = "settings.json",
    setting_sources = c("user", "project"),
    permission_prompt_tool_name = "stdio",
    plugins = c("/plugins/one"),
    extra_args = list(foo = "bar", dry_run = NULL)
  )$to_cli_args()

  expect_true("--tools" %in% args)
  expect_true("Read,Write" %in% args)
  expect_true("--max-thinking-tokens" %in% args)
  expect_true("2048" %in% args)
  expect_true("--max-budget-usd" %in% args)
  expect_true("1.5" %in% args)
  expect_true("--fallback-model" %in% args)
  expect_true("--append-system-prompt" %in% args)
  expect_true("--continue" %in% args)
  expect_true("--fork-session" %in% args)
  expect_true("--include-partial-messages" %in% args)
  expect_true("--agents" %in% args)
  expect_true("--json-schema" %in% args)
  expect_equal(sum(args == "--add-dir"), 2)
  expect_true("--settings" %in% args)
  expect_true("--setting-sources" %in% args)
  expect_true("user,project" %in% args)
  expect_true("--permission-prompt-tool" %in% args)
  expect_true("--plugin-dir" %in% args)
  expect_true("--foo" %in% args)
  expect_true("--dry_run" %in% args)
})

# ---------------------------------------------------------------------------
# Message parsing
# ---------------------------------------------------------------------------

test_that("parse_stream_line handles valid JSON", {
  line <- '{"type":"assistant","message":{"content":[{"type":"text","text":"Hello"}]}}'
  msg  <- claudeAgentR:::parse_stream_line(line)
  expect_equal(msg$type, "assistant")
})

test_that("parse_stream_line returns NULL for empty lines", {
  expect_null(claudeAgentR:::parse_stream_line(""))
  expect_null(claudeAgentR:::parse_stream_line("   "))
})

test_that("parse_stream_line returns NULL and warns on invalid JSON", {
  expect_warning(
    result <- claudeAgentR:::parse_stream_line("{not valid json"),
    "Failed to parse"
  )
  expect_null(result)
})

test_that("parse_stream_line enforces max_buffer_size", {
  line <- jsonlite::toJSON(
    list(type = "assistant", message = list(content = list(list(type = "text", text = "Hello")))),
    auto_unbox = TRUE
  )
  expect_warning(
    result <- claudeAgentR:::parse_stream_line(line, max_buffer_size = 10),
    "larger than max_buffer_size"
  )
  expect_null(result)
})

test_that("extract_text concatenates text blocks", {
  msg <- list(
    type    = "assistant",
    message = list(content = list(
      list(type = "text", text = "Hello"),
      list(type = "text", text = " world")
    ))
  )
  expect_equal(claudeAgentR:::extract_text(msg), "Hello world")
})

test_that("extract_text returns empty string when no text blocks", {
  msg <- list(
    type    = "assistant",
    message = list(content = list(
      list(type = "tool_use", name = "Bash", input = list(command = "ls"))
    ))
  )
  expect_equal(claudeAgentR:::extract_text(msg), "")
})

test_that("extract_text returns empty string when message is NULL", {
  msg <- list(type = "assistant", message = NULL)
  expect_equal(claudeAgentR:::extract_text(msg), "")
})

test_that("extract_text returns empty string when content is NULL", {
  msg <- list(type = "assistant", message = list(content = NULL))
  expect_equal(claudeAgentR:::extract_text(msg), "")
})

test_that("is_result_message detects result type", {
  expect_true(claudeAgentR:::is_result_message(list(type = "result")))
  expect_false(claudeAgentR:::is_result_message(list(type = "assistant")))
  expect_false(claudeAgentR:::is_result_message(list(type = "system")))
})

test_that("extract_session_id prefers result session id and falls back to init message", {
  from_result <- claudeAgentR:::extract_session_id(list(
    result = list(session_id = "result-session"),
    messages = list()
  ))
  expect_equal(from_result, "result-session")

  from_init <- claudeAgentR:::extract_session_id(list(
    result = list(),
    messages = list(list(type = "system", subtype = "init", session_id = "init-session"))
  ))
  expect_equal(from_init, "init-session")
})

test_that("is_tool_use_message detects tool_use blocks", {
  msg_with_tool <- list(
    type    = "assistant",
    message = list(content = list(
      list(type = "tool_use", name = "Bash", input = list(command = "ls"))
    ))
  )
  msg_text_only <- list(
    type    = "assistant",
    message = list(content = list(
      list(type = "text", text = "Just text")
    ))
  )
  msg_no_message <- list(type = "assistant", message = NULL)

  expect_true(claudeAgentR:::is_tool_use_message(msg_with_tool))
  expect_false(claudeAgentR:::is_tool_use_message(msg_text_only))
  expect_false(claudeAgentR:::is_tool_use_message(msg_no_message))
  expect_false(claudeAgentR:::is_tool_use_message(list(type = "result")))
})

test_that("print_message handles NULL msg gracefully", {
  expect_no_error(print_message(NULL))
  expect_no_error(print_message(list(type = NULL)))
})

test_that("print_message handles result type", {
  msg <- list(type = "result", subtype = "success", total_cost_usd = 0.001)
  expect_no_error(print_message(msg))
})

test_that("claude_execute returns total_cost_usd and session_id", {
  local_mocked_bindings(
    start_transport = function(prompt, options) {
      list(
        is_alive = function() FALSE,
        kill = function() NULL
      )
    },
    collect_messages = function(proc, hooks = NULL, on_message = NULL, timeout = 300, max_buffer_size = 1024 * 1024) {
      list(
        text = "Hello",
        messages = list(list(type = "system", subtype = "init", session_id = "session-456")),
        result = list(subtype = "success", total_cost_usd = 0.123, session_id = "session-456")
      )
    },
    .package = "claudeAgentR"
  )

  res <- claude_execute("Hello")
  expect_equal(res$text, "Hello")
  expect_equal(res$cost_usd, 0.123)
  expect_equal(res$subtype, "success")
  expect_equal(res$session_id, "session-456")
})

test_that("ClaudeClient resumes session on follow-up turns", {
  started <- list()

  local_mocked_bindings(
    start_transport = function(prompt, options) {
      started[[length(started) + 1]] <<- list(
        prompt = prompt,
        resume_session_id = options$resume_session_id
      )
      structure(list(is_alive = function() FALSE, kill = function() NULL), class = "mock_proc")
    },
    collect_messages = function(proc, hooks = NULL, on_message = NULL, timeout = 300, max_buffer_size = 1024 * 1024) {
      idx <- length(started)
      sid <- if (idx == 1) "session-1" else started[[idx]]$resume_session_id
      list(
        text = paste("turn", idx),
        messages = list(list(type = "system", subtype = "init", session_id = sid)),
        result = list(type = "result", subtype = "success", session_id = sid, total_cost_usd = idx)
      )
    },
    .package = "claudeAgentR"
  )

  client <- claude_client()
  expect_equal(client$connect_text("first"), "turn 1")
  expect_equal(client$query_text("second"), "turn 2")
  expect_equal(started[[1]]$resume_session_id, NULL)
  expect_equal(started[[2]]$resume_session_id, "session-1")
  client$close()
})

test_that("ClaudeAsyncClient resumes session on follow-up turns", {
  started <- list()

  local_mocked_bindings(
    start_transport = function(prompt, options) {
      started[[length(started) + 1]] <<- list(
        prompt = prompt,
        resume_session_id = options$resume_session_id
      )
      sid <- if (length(started) == 1) "async-session-1" else options$resume_session_id
      lines <- c(
        jsonlite::toJSON(list(type = "system", subtype = "init", session_id = sid), auto_unbox = TRUE),
        jsonlite::toJSON(list(type = "assistant", message = list(content = list(list(type = "text", text = paste("turn", length(started)))))), auto_unbox = TRUE),
        jsonlite::toJSON(list(type = "result", subtype = "success", session_id = sid, total_cost_usd = length(started)), auto_unbox = TRUE)
      )
      state <- new.env(parent = emptyenv())
      state$lines <- lines
      list(
        poll_io = function(timeout) list(output = if (length(state$lines) > 0) "ready" else "silent"),
        read_output_lines = function(n = 100) {
          out <- state$lines
          state$lines <- character(0)
          out
        },
        is_alive = function() TRUE,
        read_all_error = function() "",
        kill = function() NULL
      )
    },
    .package = "claudeAgentR"
  )

  client <- claude_async_client()
  r1 <- NULL
  r2 <- NULL
  drain_loop <- function(predicate, max_iter = 20) {
    for (i in seq_len(max_iter)) {
      later::run_now(timeout = 0.05)
      if (predicate()) return(invisible(TRUE))
    }
    invisible(FALSE)
  }

  promises::`%...>%`(client$connect("first"), (function(value) r1 <<- value))
  expect_true(drain_loop(function() !is.null(r1)))
  promises::`%...>%`(client$query("second"), (function(value) r2 <<- value))
  expect_true(drain_loop(function() !is.null(r2)))

  expect_equal(r1$text, "turn 1")
  expect_equal(r2$text, "turn 2")
  expect_equal(started[[1]]$resume_session_id, NULL)
  expect_equal(started[[2]]$resume_session_id, "async-session-1")
  client$close()
})

test_that("print_message handles unknown type", {
  expect_no_error(print_message(list(type = "unknown_future_type")))
})

# ---------------------------------------------------------------------------
# HookRegistry
# ---------------------------------------------------------------------------

test_that("hook_allow returns allow action", {
  result <- hook_allow()
  expect_equal(result$action, "allow")
})

test_that("hook_block returns block action with default reason", {
  result <- hook_block()
  expect_equal(result$action, "block")
  expect_true(nzchar(result$reason))
})

test_that("hook_block returns block action with custom reason", {
  result <- hook_block("Too dangerous")
  expect_equal(result$action, "block")
  expect_equal(result$reason, "Too dangerous")
})

test_that("permission helpers normalize control protocol responses", {
  expect_equal(
    claudeAgentR:::normalize_permission_result(permission_allow()),
    list(behavior = "allow")
  )
  expect_equal(
    claudeAgentR:::normalize_permission_result(permission_deny("No")),
    list(behavior = "deny", message = "No")
  )
  expect_equal(
    claudeAgentR:::normalize_permission_result(hook_block("Blocked")),
    list(behavior = "deny", message = "Blocked")
  )
})

test_that("HookRegistry blocks on matching pre-hook", {
  reg <- HookRegistry$new()
  reg$register_pre_tool_use("Bash", function(input) {
    if (grepl("rm -rf", input$command %||% "")) hook_block("Blocked") else hook_allow()
  })

  expect_equal(
    reg$run_pre_hooks("Bash", list(command = "rm -rf /"))$action,
    "block"
  )
  expect_equal(
    reg$run_pre_hooks("Bash", list(command = "ls -la"))$action,
    "allow"
  )
})

test_that("HookRegistry allows when no hook registered", {
  reg    <- HookRegistry$new()
  result <- reg$run_pre_hooks("UnknownTool", list())
  expect_equal(result$action, "allow")
})

test_that("HookRegistry rejects non-function callbacks for pre-hook", {
  reg <- HookRegistry$new()
  expect_error(reg$register_pre_tool_use("Bash", "not a function"), "function")
})

test_that("HookRegistry rejects non-function callbacks for post-hook", {
  reg <- HookRegistry$new()
  expect_error(reg$register_post_tool_use("Bash", 42), "function")
})

test_that("HookRegistry post-hook fires and receives input and output", {
  reg     <- HookRegistry$new()
  log     <- list()
  reg$register_post_tool_use("Read", function(ctx) {
    log[[length(log) + 1]] <<- ctx
  })
  reg$run_post_hooks("Read", list(file_path = "foo.R"), "file contents")
  expect_length(log, 1)
  expect_equal(log[[1]]$input$file_path, "foo.R")
  expect_equal(log[[1]]$output, "file contents")
})

test_that("HookRegistry multiple pre-hooks: first block wins", {
  reg <- HookRegistry$new()
  reg$register_pre_tool_use("Bash", function(input) hook_block("first"))
  reg$register_pre_tool_use("Bash", function(input) hook_allow())

  result <- reg$run_pre_hooks("Bash", list(command = "ls"))
  expect_equal(result$action, "block")
  expect_equal(result$reason, "first")
})

test_that("HookRegistry pre-hook error does not propagate; returns allow", {
  reg <- HookRegistry$new()
  reg$register_pre_tool_use("Bash", function(input) stop("boom"))

  expect_warning(
    result <- reg$run_pre_hooks("Bash", list()),
    "Pre-hook callback error"
  )
  expect_equal(result$action, "allow")
})

test_that("HookRegistry post-hook error does not propagate", {
  reg <- HookRegistry$new()
  reg$register_post_tool_use("Write", function(ctx) stop("boom"))

  expect_warning(
    reg$run_post_hooks("Write", list(), NULL),
    "Post-hook callback error"
  )
})

test_that("HookRegistry register returns self invisibly (chainable)", {
  reg <- HookRegistry$new()
  result <- reg$register_pre_tool_use("Bash", function(input) hook_allow())
  expect_identical(result, reg)
})

# ---------------------------------------------------------------------------
# ClaudeClient (structural / non-integration)
# ---------------------------------------------------------------------------

test_that("ClaudeClient initialises without error", {
  client <- ClaudeClient$new()
  expect_s3_class(client, "R6")
  expect_null(client$last_text)
  expect_null(client$last_messages)
  expect_null(client$last_result)
})

test_that("claude_client() returns a ClaudeClient", {
  client <- claude_client(model = "claude-sonnet-4-20250514")
  expect_true(inherits(client, "ClaudeClient"))
})

test_that("ClaudeClient$query errors when not connected", {
  client <- ClaudeClient$new()
  expect_error(client$query("hello"), "Not connected")
})

test_that("ClaudeClient$connect errors when called twice", {
  client      <- ClaudeClient$new()
  private_env <- client$.__enclos_env__$private
  unlockBinding("connected", private_env)
  private_env$connected <- TRUE
  expect_error(client$connect("hello"), "Already connected")
})

test_that("ClaudeClient$close is safe when never connected", {
  client <- ClaudeClient$new()
  expect_no_error(client$close())
})

test_that("claude_client passes max_tokens to options", {
  client <- claude_client(max_tokens = 256)
  opts   <- environment(client$connect)$private$options
  expect_equal(opts$max_tokens, 256)
})

test_that("claude_client passes mcp_servers to options", {
  servers <- list(s1 = list(command = "node", args = list("s.js")))
  client  <- claude_client(mcp_servers = servers)
  opts    <- environment(client$connect)$private$options
  expect_equal(opts$mcp_servers, servers)
})

# ---------------------------------------------------------------------------
# ClaudeAsyncClient (structural)
# ---------------------------------------------------------------------------

test_that("claude_async_client() returns ClaudeAsyncClient", {
  client <- claude_async_client()
  expect_true(inherits(client, "ClaudeAsyncClient"))
})

test_that("ClaudeAsyncClient has last_text/last_messages/last_result fields", {
  client <- claude_async_client()
  expect_null(client$last_text)
  expect_null(client$last_messages)
  expect_null(client$last_result)
})

test_that("ClaudeAsyncClient$query returns promise when not connected", {
  client <- claude_async_client()
  p <- client$query("hello")
  expect_true(promises::is.promise(p))
})

test_that("ClaudeAsyncClient$close is safe when never connected", {
  client <- claude_async_client()
  expect_no_error(client$close())
})

test_that("claude_async_client passes max_tokens to options", {
  client <- claude_async_client(max_tokens = 128)
  opts   <- environment(client$connect)$private$options
  expect_equal(opts$max_tokens, 128)
})

# ---------------------------------------------------------------------------
# find_claude_cli (environment-variable path)
# ---------------------------------------------------------------------------

test_that("find_claude_cli honours CLAUDE_CLI_PATH env var", {
  # Create a temp file to act as the CLI
  fake_cli <- tempfile()
  file.create(fake_cli)
  withr::with_envvar(
    list(CLAUDE_CLI_PATH = fake_cli),
    expect_equal(claudeAgentR:::find_claude_cli(), fake_cli)
  )
  unlink(fake_cli)
})

test_that("find_claude_cli errors when CLI not found", {
  withr::with_envvar(
    list(CLAUDE_CLI_PATH = "", PATH = ""),
    expect_error(claudeAgentR:::find_claude_cli(), "Claude Code CLI not found")
  )
})

# ---------------------------------------------------------------------------
# start_transport — prompt validation
# ---------------------------------------------------------------------------

test_that("start_transport rejects empty prompt", {
  opts <- ClaudeOptions$new()
  expect_error(claudeAgentR:::start_transport("", opts),   "non-empty")
  expect_error(claudeAgentR:::start_transport("   ", opts), "non-empty")
})

test_that("start_transport rejects NULL prompt", {
  opts <- ClaudeOptions$new()
  expect_error(claudeAgentR:::start_transport(NULL, opts), "non-empty")
})

# ---------------------------------------------------------------------------
# send_message — prompt validation
# ---------------------------------------------------------------------------

test_that("send_message rejects empty prompt without a real process", {
  # We don't need a real process to test prompt validation
  fake_proc <- list(
    is_alive    = function() TRUE,
    write_input = function(...) invisible(NULL)
  )
  expect_error(claudeAgentR:::send_message(fake_proc, ""),   "non-empty")
  expect_error(claudeAgentR:::send_message(fake_proc, "  "), "non-empty")
})

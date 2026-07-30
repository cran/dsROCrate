test_that("print.safe_symbol displays populated fields", {
  x <- safe_symbol(
    symbol = "D",
    kind = "table",
    asset = "project.table",
    expr = NA_character_,
    session = "session-1"
  )

  expect_message(print(x), "<safe_symbol>")
  expect_message(print(x), "Symbol : D")
  expect_message(print(x), "Kind   : table")
  expect_message(print(x), "Asset  : project.table")
  expect_message(print(x), "Session : session-1")
})

test_that("print.safe_symbol omits fields that are NA", {
  # regression test: print.safe_symbol previously checked `!is.null(x$asset)`
  # (etc.), but these fields default to NA_character_ rather than NULL, so
  # the check never actually skipped an unset field. It also referenced
  # `x$column`/`x$parent`, which don't exist on a safe_symbol object.
  x <- safe_symbol(symbol = "D", kind = "table", asset = "project.table")

  expect_message(print(x), "<safe_symbol>")

  msg <- capture.output(print(x), type = "message")
  full_msg <- paste(msg, collapse = "\n")

  expect_false(grepl("Expression", full_msg))
  expect_false(grepl("Session", full_msg))
  expect_false(grepl("Column", full_msg))
  expect_false(grepl("Parent", full_msg))
})

test_that("print.safe_symbol returns its input invisibly", {
  x <- safe_symbol(symbol = "D", kind = "table")

  expect_identical(suppressMessages(print(x)), x)
})

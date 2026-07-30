test_that("register_symbol adds a symbol to an empty registry", {
  registry <- symbol_registry()

  symbol <- safe_symbol(
    symbol = "D",
    kind = "table",
    asset = "project.table",
    created_at = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    user = "alice",
    session = "session-1"
  )

  registry <- register_symbol(registry, symbol)

  expect_s3_class(registry, "symbol_registry")
  expect_equal(nrow(registry$symbols), 1)
  expect_equal(registry$symbols$version, 1L)
})

test_that("as.data.frame.symbol_registry does not error on a populated registry", {
  # regression test: as.data.frame.symbol_registry previously used
  # `lapply(x$symbols, function(sym) ...)`, which iterates over the
  # *columns* of the underlying tibble rather than its rows, so `sym`
  # was a plain vector and `sym$symbol` errored with "$ operator is
  # invalid for atomic vectors" as soon as the registry had any symbols.
  registry <- symbol_registry()

  symbol <- safe_symbol(
    symbol = "D",
    kind = "table",
    asset = "project.table",
    created_at = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    user = "alice",
    session = "session-1"
  )

  registry <- register_symbol(registry, symbol)

  expect_no_error(res <- as.data.frame(registry))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
})

test_that("as.data.frame.symbol_registry returns the expected, correctly-populated columns", {
  # regression test: the id column was previously populated from
  # `sym$symbol` instead of `sym$id`, and the method referenced
  # non-existent `parent`/`column` fields.
  registry <- symbol_registry()

  symbol <- safe_symbol(
    symbol = "D",
    kind = "table",
    asset = "project.table",
    created_at = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    user = "alice",
    session = "session-1"
  )

  registry <- register_symbol(registry, symbol)

  res <- as.data.frame(registry)

  expect_true(all(
    c(
      "id",
      "symbol",
      "version",
      "kind",
      "asset",
      "created_by",
      "user",
      "session"
    ) %in%
      names(res)
  ))
  expect_false(any(c("parent", "column") %in% names(res)))

  expect_equal(res$id, symbol$id)
  expect_equal(res$symbol, "D")
  expect_false(identical(res$id, res$symbol))
})

test_that("as.data.frame.symbol_registry returns an empty data frame for an empty registry", {
  registry <- symbol_registry()

  res <- as.data.frame(registry)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

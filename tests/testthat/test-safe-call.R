test_that("safe_call.character parses a function call into a safe_call", {
  x <- safe_call(
    "mean(D$age)",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )

  expect_s3_class(x, "safe_call")
  expect_equal(x$fx, "mean")
  expect_equal(x$user, "alice")
  expect_equal(x$session, "session-1")
  expect_equal(x$profile, "default")
})

test_that("as.data.frame.safe_call returns the expected columns", {
  x <- safe_call(
    "mean(D$age)",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )

  res <- as.data.frame(x)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_setequal(
    names(res),
    c("timestamp", "user", "r_cmd", "fx", "args", "session", "profile")
  )
  expect_equal(res$user, "alice")
  expect_equal(res$session, "session-1")
  expect_equal(res$profile, "default")
  expect_equal(res$timestamp, "2026-07-01T10:00:00")
  expect_true(is.list(res$args[[1]]))
  expect_equal(res$args[[1]], x$args)
})

test_that("as.data.frame.safe_call keeps a stable 'args' column regardless of arity", {
  # regression test: `args = x$args` inside data.frame() lets R recurse into
  # the named list and expand/rename it, so a single-argument call collapses
  # the "args" column into a column named after that argument (e.g. "x" for
  # `mean(x)`), and a multi-argument call spreads into several columns
  # (e.g. "args.x", "args.y") instead of one stable "args" column.
  zero_arg <- safe_call(
    "Sys.time()",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )
  one_arg <- safe_call(
    "mean(D$age)",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )
  two_arg <- safe_call(
    "seq(1, 10)",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )

  expect_true("args" %in% names(as.data.frame(zero_arg)))
  expect_true("args" %in% names(as.data.frame(one_arg)))
  expect_true("args" %in% names(as.data.frame(two_arg)))
})

test_that("as.data.frame.safe_call is not shadowed by a conflicting definition", {
  # regression test: safe-call.R and safe-call-utils.R previously both
  # defined `as.data.frame.safe_call()` with different (incompatible) sets
  # of columns; whichever file was sourced last silently won. This checks
  # that only the `timestamp`/`user`/`r_cmd`/... version is in effect.
  x <- safe_call(
    "mean(D$age)",
    `@timestamp` = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
    username = "alice",
    ds_id = "session-1",
    ds_profile = "default"
  )

  res <- as.data.frame(x)

  expect_false(any(c("package", "argument", "value") %in% names(res)))
})

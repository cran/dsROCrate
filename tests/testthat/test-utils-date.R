test_that("parse_utc() reads backend timestamp formats as UTC", {
  expected <- as.POSIXct("2026-10-13 04:26:14", tz = "UTC")
  withr::local_timezone("America/New_York")

  expect_equal(
    as.numeric(parse_utc("2026-10-13T04:26:14.076468641Z")),
    as.numeric(expected) + 0.076468,
    tolerance = 1e-3
  )
  expect_equal(parse_utc("2026-10-13T04:26:14+0000"), expected)
  expect_equal(parse_utc("2026-10-13T06:26:14+02:00"), expected)
  expect_equal(parse_utc("2026-10-13 04:26:14"), expected)
  expect_equal(parse_utc(expected), expected)
  expect_true(is.na(parse_utc("not a date")))
})

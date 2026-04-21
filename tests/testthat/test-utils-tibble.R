test_that("unfill_vec replaces repeated values with default val", {
  x <- c("a", "a", "b", "b", "b", "c")

  expect_equal(
    unfill_vec(x),
    c("a", "", "b", "", "", "c")
  )
})

test_that("unfill_vec respects custom val", {
  x <- c("x", "x", "y", "y")

  expect_equal(
    unfill_vec(x, val = NA_character_),
    c("x", NA, "y", NA)
  )
})

test_that("unfill_vec handles NA values correctly", {
  x <- c("a", NA, NA, "b", "b")

  expect_equal(
    unfill_vec(x),
    c("a", NA, NA, "b", "")
  )
})

test_that("unfill_vec works with length-1 vectors", {
  expect_equal(
    unfill_vec("a"),
    "a"
  )
})


test_that("refill_vec fills down missing or default values", {
  x <- c("a", "", "", "b", "", "c")

  expect_equal(
    refill_vec(x),
    c("a", "a", "a", "b", "b", "c")
  )
})

test_that("refill_vec fills down NA values", {
  x <- c("a", NA, NA, "b", NA)

  expect_equal(
    refill_vec(x),
    c("a", "a", "a", "b", "b")
  )
})

test_that("refill_vec respects custom val", {
  x <- c("a", "-", "-", "b", "-")

  expect_equal(
    refill_vec(x, val = "-"),
    c("a", "a", "a", "b", "b")
  )
})

test_that("refill_vec leaves first element unchanged", {
  x <- c("", "a", "")

  expect_equal(
    refill_vec(x),
    c("", "a", "a")
  )
})

test_that("refill_vec works with length-1 vectors", {
  expect_equal(
    refill_vec("a"),
    "a"
  )

  expect_equal(
    refill_vec(NA_character_),
    NA_character_
  )
})

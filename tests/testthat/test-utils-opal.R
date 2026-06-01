test_that("validate_con works with real Opal connection", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_no_error(validate_con(opal_con))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("is_opal_admin_con detects admin connection correctly", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  res <- is_opal_admin_con(opal_con)

  expect_type(res, "logical")
  expect_true(res)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("validate_con errors on invalid connection object", {
  bad_con <- list(handle = list(handle = NULL))

  expect_error(
    validate_con(bad_con),
    "Unsupported connection type"
  )
})

test_that("user_perm_entity works for all values of 'permission'", {
  input_tbl <- tibble::tibble(
    person = "dsuser",
    person_id = "dsuser",
    asset = "tab1",
    asset_id = "tab1",
    permission = c(
      "view",
      "view-values",
      "edit",
      "edit-values",
      "administrate"
    )
  )

  expect_no_error(
    output_tbl <- purrr::pmap(input_tbl, user_perm_entity)
  )

  expect_length(output_tbl, 5)
  expect_type(output_tbl, "list")
})

test_that("user_perm_entity returns NULL for unknown 'permission'", {
  input_tbl <- tibble::tibble(
    person = "dsuser",
    person_id = "dsuser",
    asset = "tab1",
    asset_id = "tab1",
    permission = "INVALID"
  )

  expect_no_error(
    output_tbl <- purrr::pmap(input_tbl, user_perm_entity)
  )

  expect_null(output_tbl[[1]])
  expect_length(output_tbl, 1)
  expect_type(output_tbl, "list")
})

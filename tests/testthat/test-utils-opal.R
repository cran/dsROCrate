test_that("validate_opal_con works with real Opal connection", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_no_error(validate_opal_con(opal_con))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("is_opal_admin_con detects admin group correctly", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  res <- is_opal_admin_con(opal_con)

  expect_type(res, "logical")
  expect_true(res)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_project_tables retrieves real tables from demo server", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  project <- attr(opal_con, "PROJECT")

  tables <- get_project_tables(opal_con, project)

  expect_type(tables, "character")
  expect_true(length(tables) >= 1)
  expect_true(all(nzchar(tables)))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_project_details works end-to-end with demo Opal project", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  project <- attr(opal_con, "PROJECT")

  res <- get_project_details(opal_con, project)

  expect_s3_class(res, "data.frame")
  expect_true("project" %in% names(res))
  expect_true("table" %in% names(res))
  expect_true(nrow(res) >= 1)
  expect_true(all(res$project == project))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_table_permissions errors for non-admin connection", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con(admin = FALSE)
  project <- attr(opal_con, "PROJECT")
  tables <- attr(opal_con, "TABLES")

  expect_error(
    get_table_permissions(opal_con, project, tables),
    "The provided connection does not have access to retrieve table permissions!"
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_table_permissions throws a warning for an invalid project", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  tables <- attr(opal_con, "TABLES")

  expect_warning(
    get_table_permissions(opal_con, "INVALID PROJECT", tables),
    "404"
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_table_permissions retrieves real permissions", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  project <- attr(opal_con, "PROJECT")
  tables <- attr(opal_con, "TABLES")

  res <- get_table_permissions(opal_con, project, tables)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("project", "table") %in% names(res)))
  expect_equal(unique(res$project), project)
  expect_equal(unique(res$table), tables)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("get_project_details handles non-existent project gracefully", {
  opal_con <- opal_demo_con()

  res <- get_project_details(opal_con, "THIS_PROJECT_DOES_NOT_EXIST_123")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("validate_opal_con errors on invalid connection object", {
  bad_con <- list(handle = list(handle = NULL))

  expect_error(
    validate_opal_con(bad_con),
    "connection is not valid"
  )
})

test_that("update_project_datasets updates project entities of an RO-Crate", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  # create basic RO-Crate with a project
  rocrate <- rocrateR::rocrate_5s() |>
    dsROCrate::safe_project(
      connection = opal_con,
      project = attr(opal_con, "PROJECT")
    )

  # update project datasets
  expect_no_error(
    rocrate <- rocrate |>
      update_project_datasets(project = attr(opal_con, "PROJECT"), ds_ids = 1:5)
  )

  # extract `hasPart` for the project entity
  expect_no_error(
    has_part <- rocrateR::get_entity(rocrate, type = "Project") |>
      sapply(getElement, name = "hasPart") |>
      sapply(getElement, name = "@id") |>
      unlist()
  )

  expect_equal(length(has_part), 5)
  expect_equal(has_part, 1:5)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
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

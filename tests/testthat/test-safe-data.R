# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - ERRORS - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_data fails when attempt calling function with invalid class", {
  expect_error(
    dsROCrate::safe_data(structure(list(), class = "InvalidClass"))
  )
})

test_that("safe_data fails when attempt calling with invalid connection", {
  # setup
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_data(connection = NULL)
  )
})

test_that("safe_data fails when attempt adding invalid project", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_data(project = "Invalid Project", connection = opal_con)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_data fails when attempt calling without a project", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_data(connection = opal_con)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_data fails when attempt calling with project = NULL", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_data(connection = opal_con, project = NULL)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_data fails when attempt calling with multiple projects", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_data(connection = opal_con, project = c("A", "B"))
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - VALID  - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_data works", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  basic_rocrate <- rocrateR::rocrate_5s()

  # add all datasets for a valid project
  basic_rocrate_2 <- basic_rocrate |>
    dsROCrate::safe_data(
      project = attr(opal_con, "PROJECT"),
      connection = opal_con
    )
  ## extract datasets
  basic_rocrate_2_sd <- basic_rocrate_2 |>
    .get_entity(type = "Dataset")
  ## extract datasets' names
  basic_rocrate_2_sd_names <- basic_rocrate_2_sd |>
    sapply(getElement, name = "name")
  ## verify that CNSIM1, CNSIM2 and CNSIM3 are part in the crate
  expect_true(all(
    c("CNSIM1", "CNSIM2", "CNSIM3") %in% basic_rocrate_2_sd_names
  ))

  # add specific datasets for a valid project
  basic_rocrate_3 <- basic_rocrate |>
    dsROCrate::safe_data(
      project = attr(opal_con, "PROJECT"),
      tables = attr(opal_con, "TABLES"),
      connection = opal_con
    )
  ## extract datasets
  basic_rocrate_3_sd <- basic_rocrate_3 |>
    .get_entity(type = "Dataset")
  ## verify the length of the datasets is equal to 1 + 1 (root directory)
  expect_equal(length(basic_rocrate_3_sd), 2)

  basic_rocrate_4 <- basic_rocrate |>
    dsROCrate::safe_data(
      project = attr(opal_con, "PROJECT"),
      connection = opal_con,
      user = "dsuser"
    )
  ## extract datasets
  basic_rocrate_4_sd <- basic_rocrate_4 |>
    .get_entity(type = "Dataset")
  ## extract datasets' names
  basic_rocrate_4_sd_names <- basic_rocrate_4_sd |>
    sapply(getElement, name = "name")
  ## verify that CNSIM1, CNSIM2 and CNSIM3 are part in the crate
  expect_true(all(
    c("CNSIM1", "CNSIM2", "CNSIM3") %in% basic_rocrate_4_sd_names
  ))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

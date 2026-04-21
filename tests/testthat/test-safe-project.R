# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - ERRORS - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_project fails when attempt calling function with invalid class", {
  expect_error(
    dsROCrate::safe_project(
      structure(list(), class = "InvalidClass"),
      project = NULL
    )
  )
})

test_that("safe_project fails when attempt calling with invalid connection", {
  # setup
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(connection = NULL, project = NULL)
  )
})

test_that("safe_project fails when attempt adding invalid project", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(
        project = "Invalid Project",
        connection = opal_con
      )
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_project fails when attempt calling without a project", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(connection = opal_con)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_project fails when attempt calling with project = NULL", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(connection = opal_con, project = NULL)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_project fails when attempt calling with multiple projects", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(connection = opal_con, project = c("A", "B"))
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - VALID  - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_project works", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  # add details for Project without Dataset entities
  basic_rocrate_2 <- basic_rocrate |>
    dsROCrate::safe_project(
      project = attr(opal_con, "PROJECT"),
      connection = opal_con
    )

  # add all datasets for a valid project
  basic_rocrate_3 <- basic_rocrate |>
    dsROCrate::safe_data(
      project = attr(opal_con, "PROJECT"),
      connection = opal_con
    ) |>
    # add the Safe Project details
    dsROCrate::safe_project(
      project = attr(opal_con, "PROJECT"),
      connection = opal_con
    )
  ## extract datasets
  basic_rocrate_3_sd <- basic_rocrate_3 |>
    rocrateR::get_entity(type = "Dataset")
  ## extract datasets' names
  basic_rocrate_3_sd_names <- basic_rocrate_3_sd |>
    sapply(getElement, name = "name")
  ## verify that CNSIM1, CNSIM2 and CNSIM3 are part in the crate
  expect_true(all(
    c("CNSIM1", "CNSIM2", "CNSIM3") %in% basic_rocrate_3_sd_names
  ))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

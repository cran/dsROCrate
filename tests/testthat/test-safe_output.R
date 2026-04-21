# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - ERRORS - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_output fails when calling with invalid object", {
  expect_error(
    dsROCrate::safe_output(structure(list(), class = "InvalidClass"))
  )
})

test_that("safe_output fails when attempt calling with invalid connection", {
  # setup
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_output(connection = NULL)
  )
})

test_that("safe_output warns when calling with RO-Crate without Safe People details", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_warning(
    basic_rocrate |>
      dsROCrate::safe_output(connection = opal_con)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("safe_output warns when attempt setting multiple users as the RO-Crate authors", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_project(
        connection = opal_con,
        project = attr(opal_con, "PROJECT")
      ) |>
      dsROCrate::safe_people(user = 1) |>
      dsROCrate::safe_people(user = 2) |>
      rocrateR::add_entity_value(
        id = "./",
        key = "author",
        value = lapply(
          c(
            "#person:6717f2823d3202449301145073ab8719",
            "#person:db8e490a925a60e62212cefc7674ca02"
          ),
          \(x) list(`@id` = x)
        )
      ) |>
      dsROCrate::safe_output(connection = opal_con, user = NULL)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - VALID  - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_output works", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  # add Safe People details
  ## add project entity to the RO-Crate
  basic_rocrate_2 <- basic_rocrate |>
    rocrateR::add_entity(rocrateR::entity("Fake Project", type = "Project"))

  ## add Safe People details for
  basic_rocrate_3 <- basic_rocrate_2 |>
    dsROCrate::safe_people(
      connection = opal_con,
      user = attr(opal_con, "PEOPLE")
    )

  # call the function with an RO-Crate with multiple users
  basic_rocrate_4 <- basic_rocrate_3 |>
    # add another username
    dsROCrate::safe_people(
      connection = opal_con,
      user = list(id = "extra_username", name = "Extra username"),
      set_author = FALSE
    ) |>
    # set multiple users as the author
    rocrateR::add_entity_value(
      id = "./",
      key = "author",
      value = list(
        list(`@id` = "extra_username"),
        list(`@id` = paste0("#person:", digest::digest("dsuser")))
      ),
      overwrite = TRUE
    )

  # test for warning about multiple authors in the root entity
  expect_warning(
    basic_rocrate_4 |>
      dsROCrate::safe_output(connection = opal_con)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)

  # simulate DataSHIELD operations
  ## run some test commands with dsBaseClient
  ### needed to defined the OpalDriver class in the current environment
  DSOpal::Opal()
  ### create new login object, note that here we use the `dsuser`
  builder <- DSI::newDSLoginBuilder()
  builder$append(
    server = "study1",
    url = attr(opal_con, "SERVER"),
    user = "dsuser",
    password = attr(opal_con, "DSUSERPASS"),
    driver = "OpalDriver"
  )
  logindata <- builder$build()
  conns <- DSI::datashield.login(logins = logindata)

  ### assign data
  DSI::datashield.assign.table(
    conns["study1"],
    symbol = "dsROCrate_test",
    table = paste0(attr(opal_con, "PROJECT"), ".", attr(opal_con, "TABLES")[1]),
    errors.print = TRUE
  )

  dsBaseClient::ds.ls(datasources = conns["study1"])

  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_warning(
    basic_rocrate_5 <- basic_rocrate_3 |>
      dsROCrate::safe_output(
        connection = opal_con,
        logs_from = Sys.time() - 60, # capture the last min
        logs_to = Sys.time(),
        user = "dsuser"
      )
  )

  # run function with logs for an invalid period (no logs)
  expect_warning(
    basic_rocrate_6 <- basic_rocrate_3 |>
      dsROCrate::safe_output(
        connection = opal_con,
        logs_from = Sys.time() - 60^2 * 24, # 1 day ago
        logs_to = Sys.time() - 60^2 * 23,
        user = "dsuser"
      )
  )

  # provide invalid path to write the logs
  expect_warning(
    basic_rocrate_7 <- basic_rocrate_3 |>
      dsROCrate::safe_output(
        connection = opal_con,
        logs_from = Sys.time() - 60^2 * 24, # 1 day ago
        logs_to = Sys.time(),
        user = "dsuser",
        path = "/invalid/path/rocrateR"
      )
  )

  # write logs in temporary file
  tempdir_name <- tempdir()
  on.exit(unlink(tempdir_name, force = TRUE, recursive = TRUE))
  expect_true(dir.exists(tempdir_name))
  ## ignore warnings about missing logs
  suppressWarnings(
    basic_rocrate_8 <- basic_rocrate_3 |>
      dsROCrate::safe_output(
        connection = opal_con,
        logs_from = Sys.time() - 60, # capture the last min
        logs_to = Sys.time(),
        user = "dsuser",
        path = tempdir_name
      )
  )
  unlink(tempdir_name, force = TRUE, recursive = TRUE)
  expect_false(dir.exists(tempdir_name))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - ERRORS - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_people fails when attempt calling function with invalid class", {
  expect_error(
    dsROCrate::safe_people(structure(list(), class = "InvalidClass"))
  )
})

test_that("safe_people fails when attempt calling with invalid connection", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  expect_error(
    basic_rocrate |>
      dsROCrate::safe_people(user = attr(opal_con, "PEOPLE"), connection = NULL)
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# - - - - - - - - - - - - - - - - - VALID  - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
test_that("safe_people works", {
  # setup
  ## open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  ## create basic RO-Crate
  basic_rocrate <- rocrateR::rocrate_5s()

  # add project entity to the RO-Crate
  basic_rocrate_2 <- basic_rocrate |>
    rocrateR::add_entity(rocrateR::entity("Fake Project", type = "Project"))

  # add default Safe People details (user logged in)
  basic_rocrate_3 <- basic_rocrate_2 |>
    dsROCrate::safe_people(connection = opal_con)
  ## retrieve safe_people entity
  basic_rocrate_2_sp <- basic_rocrate_3 |>
    rocrateR::get_entity(type = "Person")
  expect_equal(length(basic_rocrate_2_sp), 1)

  # add custom Safe People
  basic_rocrate_4 <- basic_rocrate_2 |>
    dsROCrate::safe_people(
      connection = opal_con,
      user = list(id = "test_user", name = "Test User")
    )
  ## retrieve safe_people entity
  basic_rocrate_4_sp <- basic_rocrate_4 |>
    rocrateR::get_entity(type = "Person")
  expect_equal(length(basic_rocrate_4_sp), 1)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

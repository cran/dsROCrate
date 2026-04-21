test_that("project_exists.opal() succeeds when project exists", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_silent(
    project_exists(opal_con, project = attr(opal_con, "PROJECT"))
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("project_exists.opal() errors when project does not exist", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_error(
    project_exists(opal_con, project = "THIS_PROJECT_DOES_NOT_EXIST"),
    "was not found in the given Opal connection!"
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("project_exists() dispatches to opal method", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  expect_silent(
    project_exists(opal_con, project = attr(opal_con, "PROJECT"))
  )

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

# test_that("project_exists() works for armadillo objects", {
#   skip_if_not_installed("MolgenisArmadillo")
#
#   arm <- methods::new("ArmadilloCredentials")
#
#   local_mocked_bindings(
#     armadillo.list_projects = function() c("proj1", "proj2"),
#     .package = "MolgenisArmadillo"
#   )
#
#   expect_silent(
#     project_exists(arm, project = "proj1")
#   )
#
#   expect_error(
#     project_exists(arm, project = "missing"),
#     "was not found in the given Armadillo connection!"
#   )
# })

test_that("parse_user_profiles() dispatches to opal method", {
  opal_con <- structure(list(), class = "opal")

  local_mocked_bindings(
    `opal.get` = function(...) {
      list(data.frame(principal = "user1", stringsAsFactors = FALSE))
    },
    .package = "opalr"
  )

  res <- parse_user_profiles(opal_con, user = "user1")

  expect_s3_class(res, "data.frame")
  expect_equal(res$principal, "user1")
})

test_that("parse_user_profiles.opal() returns empty tibble when no matching users", {
  opal_con <- structure(list(), class = "opal")

  local_mocked_bindings(
    `opal.get` = function(...) {
      list(data.frame(principal = "other_user", stringsAsFactors = FALSE))
    },
    .package = "opalr"
  )

  res <- parse_user_profiles(opal_con, user = "user1")

  expect_equal(nrow(res), 0)
})

test_that("parse_user_profiles.opal() handles missing userInfo column", {
  opal_con <- structure(list(), class = "opal")

  local_mocked_bindings(
    `opal.get` = function(...) {
      list(
        data.frame(
          principal = c("user1", "user2"),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "opalr"
  )

  res <- parse_user_profiles(opal_con, user = "user1")

  expect_equal(nrow(res), 1)
  expect_false("userInfo" %in% colnames(res) && is.list(res$userInfo))
})

test_that("parse_user_profiles.opal() parses JSON userInfo and NA correctly", {
  opal_con <- structure(list(), class = "opal")

  json <- '{"givenName":"John","familyName":"Doe"}'

  local_mocked_bindings(
    `opal.get` = function(...) {
      list(
        data.frame(
          principal = c("user1", "user1"),
          userInfo = c(json, NA_character_),
          stringsAsFactors = FALSE
        )
      )
    },
    .package = "opalr"
  )

  res <- parse_user_profiles(opal_con, user = "user1")

  expect_equal(nrow(res), 2)
  expect_true(is.list(res$userInfo))
  expect_s3_class(res$userInfo[[1]], "tbl_df")
  expect_equal(nrow(res$userInfo[[2]]), 0)
})

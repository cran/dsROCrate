test_that("check_permissions.default errors for unsupported classes", {
  expect_error(
    check_permissions(structure(list(), class = "not_a_backend")),
    "No `check_permissions\\(\\)` method exists"
  )
})

test_that("check_permissions.ArmadilloCredentials errors as not implemented", {
  expect_error(
    check_permissions(structure(list(), class = "ArmadilloCredentials")),
    "not currently implemented"
  )
})

test_that("check_permissions.opal succeeds for an admin connection", {
  local_mocked_bindings(
    backend_user_exists = function(x, ...) TRUE,
    backend_profile_exists = function(x, ...) TRUE
  )

  con <- fake_opal_con()

  expect_true(isTRUE(check_permissions(con)))
})

test_that("check_permissions.opal succeeds for an auditor-only connection", {
  # a pure auditor lacks admin access, so `backend_user_exists()` should
  # error (403-like), while `backend_profile_exists()` still succeeds
  local_mocked_bindings(
    backend_user_exists = function(x, ...) stop("403 Forbidden"),
    backend_profile_exists = function(x, ...) TRUE
  )

  con <- fake_opal_con()

  expect_true(isTRUE(check_permissions(con)))
})

test_that("check_permissions.opal errors when neither role is available", {
  local_mocked_bindings(
    backend_user_exists = function(x, ...) stop("403 Forbidden"),
    backend_profile_exists = function(x, ...) stop("404 Not Found")
  )

  con <- fake_opal_con()

  expect_error(check_permissions(con), "does not have sufficient permissions")
})

test_that("check_permissions.opal is silent by default on success", {
  local_mocked_bindings(
    backend_user_exists = function(x, ...) TRUE,
    backend_profile_exists = function(x, ...) TRUE
  )

  con <- fake_opal_con()

  expect_silent(check_permissions(con))
})

test_that("check_permissions.opal shows a message when verbose = TRUE", {
  local_mocked_bindings(
    backend_user_exists = function(x, ...) TRUE,
    backend_profile_exists = function(x, ...) TRUE
  )

  con <- fake_opal_con()

  expect_message(check_permissions(con, verbose = TRUE), "ready to audit")
})

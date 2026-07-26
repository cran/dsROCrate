test_that("audit_engine.default errors for unsupported classes", {
  expect_error(
    audit_engine(structure(list(), class = "not_a_backend")),
    "No `audit_engine\\(\\)` method exists"
  )
})

test_that("audit_engine.opal requires a `project` argument", {
  local_mocked_bindings(validate_backend = function(x, ...) invisible(TRUE))

  con <- fake_opal_con()

  expect_error(audit_engine(con), "A `project` name is required")
})

test_that("audit_engine.opal errors for a project not on the server", {
  local_mocked_bindings(
    validate_backend = function(x, ...) invisible(TRUE),
    backend_projects = function(x, ...) tibble::tibble(name = "REAL_PROJECT")
  )

  con <- fake_opal_con()

  expect_error(
    audit_engine(con, project = "MISSING_PROJECT"),
    "not valid"
  )
})

test_that("audit_engine.opal orchestrates the 5 Safes steps correctly", {
  # two users on the server: `alice` (regular user) and `bob_admin` (has
  # administrate permission, so should be excluded from Safe People/Output)
  local_mocked_bindings(
    validate_backend = function(x, ...) invisible(TRUE),
    backend_projects = function(x, ...) tibble::tibble(name = "PROJECT1"),
    backend_users = function(x, ..., df = FALSE) {
      list(
        list(principal = "alice", groups = list("auditor")),
        list(principal = "bob_admin", groups = list("admin"))
      )
    },
    backend_sys_perms = function(x, ...) {
      tibble::tibble(
        subject = c("alice", "bob_admin", "opal-administrator"),
        permission = c("view", "administrate", "administrate"),
        type = c("user", "user", "groups")
      )
    }
  )

  # records what each Safe * step was called with, without needing `<<-`:
  # environments are mutable by reference, so a plain `<-` inside the mock
  # closures is enough to record calls in the enclosing test's scope
  calls <- new.env()
  calls$safe_people <- character()
  calls$safe_project <- character()
  calls$safe_data <- character()
  calls$safe_output <- character()
  calls$safe_setting <- FALSE

  local_mocked_bindings(
    `safe_people.rocrate` = function(
      x,
      ...,
      connection,
      user,
      set_author = TRUE,
      set_project = TRUE
    ) {
      calls$safe_people <- c(calls$safe_people, user)
      x
    },
    `safe_project.rocrate` = function(x, ..., connection, project) {
      calls$safe_project <- c(calls$safe_project, project)
      x
    },
    `safe_data.rocrate` = function(x, ..., connection, project) {
      calls$safe_data <- c(calls$safe_data, project)
      x
    },
    `safe_output.rocrate` = function(
      x,
      ...,
      connection,
      user,
      logs_from,
      logs_to,
      path = NULL
    ) {
      calls$safe_output <- c(calls$safe_output, user)
      x
    },
    `safe_setting.opal` = function(x, ..., rocrate) {
      calls$safe_setting <- TRUE
      rocrate
    }
  )

  con <- fake_opal_con()
  result <- audit_engine(con, project = "PROJECT1")

  # `bob_admin` is excluded because of the "administrate" permission
  expect_equal(calls$safe_people, "alice")
  expect_equal(calls$safe_output, "alice")
  expect_equal(calls$safe_project, "PROJECT1")
  expect_equal(calls$safe_data, "PROJECT1")
  expect_true(calls$safe_setting)
  expect_s3_class(result, "rocrate")
})

test_that("audit_engine.opal filters Safe People by the `user` argument", {
  local_mocked_bindings(
    validate_backend = function(x, ...) invisible(TRUE),
    backend_projects = function(x, ...) tibble::tibble(name = "PROJECT1"),
    backend_users = function(x, ..., df = FALSE) {
      list(
        list(principal = "alice", groups = list("standard")),
        list(principal = "carol", groups = list("standard"))
      )
    },
    backend_sys_perms = function(x, ...) {
      tibble::tibble(
        subject = c("alice", "carol", "opal-administrator"),
        permission = c("view", "view", "administrate"),
        type = c("user", "user", "groups")
      )
    }
  )

  calls <- new.env()
  calls$safe_people <- character()

  local_mocked_bindings(
    `safe_people.rocrate` = function(
      x,
      ...,
      connection,
      user,
      set_author = TRUE,
      set_project = TRUE
    ) {
      calls$safe_people <- c(calls$safe_people, user)
      x
    },
    `safe_project.rocrate` = function(x, ...) x,
    `safe_data.rocrate` = function(x, ...) x,
    `safe_output.rocrate` = function(x, ...) x,
    `safe_setting.opal` = function(x, ..., rocrate) rocrate
  )

  con <- fake_opal_con()
  audit_engine(con, project = "PROJECT1", user = "alice")

  expect_equal(calls$safe_people, "alice")
})

test_that("audit_engine.opal errors if the `user` filter matches nobody", {
  local_mocked_bindings(
    validate_backend = function(x, ...) invisible(TRUE),
    backend_projects = function(x, ...) tibble::tibble(name = "PROJECT1"),
    backend_users = function(x, ..., df = FALSE) {
      list(
        list(principal = "alice", groups = list("standard")),
        list(principal = "carol", groups = list("standard"))
      )
    },
    backend_sys_perms = function(x, ...) {
      tibble::tibble(
        subject = c("alice", "carol", "opal-administrator"),
        permission = c("view", "view", "administrate"),
        type = c("user", "user", "groups")
      )
    }
  )

  con <- fake_opal_con()

  expect_error(
    audit_engine(con, project = "PROJECT1", user = "nobody_here"),
    "No Safe People details were found"
  )
})

test_that("audit_engine.opal aborts if Safe People details cannot be obtained", {
  # simulates e.g. a permissions-lookup failure on the server:
  # `filter_safe_people()` falls back to an empty table internally, and
  # since an audit is not meaningful without Safe People details,
  # `audit_engine.opal()` should abort rather than silently producing an
  # incomplete audit
  local_mocked_bindings(
    validate_backend = function(x, ...) invisible(TRUE),
    backend_projects = function(x, ...) tibble::tibble(name = "PROJECT1"),
    backend_users = function(x, ..., df = FALSE) {
      list(list(principal = "alice", groups = list(character(0))))
    },
    backend_sys_perms = function(x, ...) stop("500 Internal Server Error")
  )

  con <- fake_opal_con()

  expect_error(
    audit_engine(con, project = "PROJECT1"),
    "No Safe People details could be found"
  )
})

test_that("audit.default errors for unsupported classes", {
  expect_error(
    audit(structure(list(), class = "not_a_backend")),
    "No `audit\\(\\)` method exists"
  )
})

test_that("audit.ArmadilloCredentials errors as not implemented", {
  expect_error(
    audit(structure(list(), class = "ArmadilloCredentials")),
    "not currently implemented"
  )
})

test_that("audit.rocrate returns the input unchanged", {
  roc <- structure(list(marker = "unchanged"), class = "rocrate")

  expect_identical(audit(roc), roc)
})

test_that("audit.list dispatches over every element", {
  roc1 <- structure(list(marker = "one"), class = "rocrate")
  roc2 <- structure(list(marker = "two"), class = "rocrate")

  result <- audit(list(roc1, roc2))

  expect_identical(result, list(roc1, roc2))
})

test_that("audit.character errors for a non-existent file", {
  expect_error(
    audit(file.path(tempdir(), "does-not-exist.yaml")),
    "does not exist"
  )
})

test_that("audit.character surfaces both loader errors for an invalid file", {
  tmp <- withr::local_tempfile(fileext = ".yaml")
  writeLines("not: a valid cr8tor bundle or rocrate", tmp)

  err <- tryCatch(audit(tmp), error = function(e) e)

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "cr8tor bundle error:")
  expect_match(conditionMessage(err), "rocrate error:")
})

test_that("audit.character dispatches a successfully loaded cr8tor bundle", {
  stub_bundle <- structure(list(marker = "cr8tor-stub"), class = "cr8tor")

  local_mocked_bindings(
    load_cr8tor_bundle = function(x, ...) stub_bundle,
    `audit_engine.cr8tor` = function(x, ...) "AUDITED"
  )

  tmp <- withr::local_tempfile()
  file.create(tmp)

  expect_equal(audit(tmp), "AUDITED")
})

test_that("audit.opal passes project/user/log args through to audit_engine", {
  captured <- new.env()

  local_mocked_bindings(
    `audit_engine.opal` = function(
      x,
      ...,
      project = NULL,
      user = NULL,
      logs_from = -Inf,
      logs_to = Inf,
      path = NULL
    ) {
      captured$project <- project
      captured$user <- user
      captured$logs_from <- logs_from
      captured$logs_to <- logs_to
      "AUDITED"
    }
  )

  con <- fake_opal_con()
  result <- audit(
    con,
    project = "PROJECT1",
    user = "alice",
    logs_from = 100,
    logs_to = 200
  )

  expect_equal(result, "AUDITED")
  expect_equal(captured$project, "PROJECT1")
  expect_equal(captured$user, "alice")
  expect_equal(captured$logs_from, 100)
  expect_equal(captured$logs_to, 200)
})

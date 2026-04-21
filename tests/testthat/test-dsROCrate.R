test_that("init() dispatches to init.opal() with real Opal connection", {
  # setup
  crate <- rocrateR::rocrate_5s()
  opal_con <- opal_demo_con()

  res <- init(
    opal_con,
    rocrate = crate,
    project = attr(opal_con, "PROJECT"),
    tables = attr(opal_con, "TABLES"),
    user = attr(opal_con, "PEOPLE")
  )

  expect_s3_class(res, "rocrate")
  expect_identical(attr(res, "connection"), opal_con)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("init.opal() attaches metadata using demo Opal server", {
  # setup
  crate <- rocrateR::rocrate_5s()
  opal_con <- opal_demo_con()

  res <- init.opal(
    opal_con,
    rocrate = crate,
    path = tempdir(),
    project = attr(opal_con, "PROJECT"),
    tables = attr(opal_con, "TABLES"),
    user = attr(opal_con, "PEOPLE")
  )

  expect_identical(attr(res, "connection"), opal_con)
  expect_identical(attr(res, "project"), attr(opal_con, "PROJECT"))
  expect_identical(attr(res, "tables"), attr(opal_con, "TABLES"))
  expect_identical(attr(res, "user"), attr(opal_con, "PEOPLE"))
  expect_true(dir.exists(attr(res, "path")))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("init.rocrate() reuses stored attributes with demo Opal server", {
  # setup
  crate <- rocrateR::rocrate_5s()
  opal_con <- opal_demo_con()

  attr(crate, "connection") <- opal_con
  attr(crate, "project") <- attr(opal_con, "PROJECT")
  attr(crate, "tables") <- attr(opal_con, "TABLES")
  attr(crate, "user") <- attr(opal_con, "PEOPLE")
  attr(crate, "path") <- tempdir()

  res <- init(crate)

  expect_identical(attr(res, "connection"), opal_con)
  expect_identical(attr(res, "project"), attr(opal_con, "PROJECT"))
  expect_identical(attr(res, "tables"), attr(opal_con, "TABLES"))
  expect_identical(attr(res, "user"), attr(opal_con, "PEOPLE"))
  expect_identical(attr(res, "path"), attr(crate, "path"))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("init.opal() errors on invalid connection object", {
  # setup
  crate <- rocrateR::rocrate_5s()

  expect_error(
    init.opal(list(not = "opal"), rocrate = crate),
    class = "error"
  )
})

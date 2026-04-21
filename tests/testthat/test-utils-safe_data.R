test_that("extract_safe_data.opal returns a valid RO-Crate with datasets", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  # terminate connection when done with tests
  withr::defer(opalr::opal.logout(opal_con))

  # create local mock binding, to avoid long run times
  local_mocked_bindings(
    opal.datasources = function(...) {
      tibble::tibble(name = attr(opal_con, "PROJECT"))
    },
    .package = "opalr"
  )

  # ignore warning of project not having tables associated
  suppressWarnings(
    roc <- extract_safe_data(opal_con)
  )

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(roc))

  # should contain Dataset entities (if demo server has datasources)
  datasets <- rocrateR::get_entity(roc, type = "Dataset")

  expect_type(datasets, "list")
  expect_true(length(datasets) >= 1)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("extract_safe_data.opal updates an existing RO-Crate", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  # terminate connection when done with tests
  withr::defer(opalr::opal.logout(opal_con))

  base_roc <- rocrateR::rocrate_5s()

  # create local mock binding, to avoid long run times
  local_mocked_bindings(
    opal.datasources = function(...) {
      tibble::tibble(name = attr(opal_con, "PROJECT"))
    },
    .package = "opalr"
  )

  # ignore warning of project not having tables associated
  suppressWarnings(
    roc <- extract_safe_data(opal_con, rocrate = base_roc)
  )

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(roc))

  datasets <- rocrateR::get_entity(roc, type = "Dataset")
  expect_true(length(datasets) >= 1)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("extract_safe_data.rocrate copies Dataset entities", {
  # create source crate with Dataset entities
  src <- rocrateR::rocrate_5s()

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#asset:1",
      type = "Dataset",
      name = "Dataset 1"
    )
  )

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#asset:2",
      type = "Dataset",
      name = "Dataset 2"
    )
  )

  # ignore warning of project not having tables associated
  suppressWarnings(
    new_roc <- extract_safe_data(src)
  )

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(new_roc))

  ents <- rocrateR::get_entity(new_roc, type = "Dataset")
  expect_equal(length(ents), 3) # 2 + 1 root entity
})

test_that("extract_safe_data.rocrate filters Dataset entities by id", {
  # create source crate with Dataset entities
  src <- rocrateR::rocrate_5s()

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#asset:keep_me",
      type = "Dataset",
      name = "Keep"
    )
  )

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#asset:drop_me",
      type = "Dataset",
      name = "Drop"
    )
  )

  # ignore warning of project not having tables associated
  suppressWarnings(
    new_roc <- extract_safe_data(src, id = "#asset:keep_me")
  )

  ents <- rocrateR::get_entity(new_roc, type = "Dataset")
  ids <- vapply(ents, function(e) e[["@id"]], character(1))

  expect_equal(ids, c("./", "#asset:keep_me"))
})

test_that("extract_safe_data.rocrate errors when no Dataset entities exist", {
  # create source crate with Dataset entities
  src <- rocrateR::rocrate_5s()

  expect_error(
    extract_safe_data(src),
    "No matching entities were found"
  )
})

test_that("flatten_safe_data.default returns empty tibble", {
  res <- flatten_safe_data(list())

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("flatten_safe_data.rocrate extracts id and name correctly", {
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "#asset:1",
      type = "Dataset",
      name = "Dataset One"
    )
  )

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "#asset:2",
      type = "Dataset",
      name = "Dataset Two"
    )
  )

  res <- flatten_safe_data(roc)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("asset_id", "asset") %in% names(res)))
  expect_equal(nrow(res), 2)
})

test_that("flatten_safe_data.rocrate filters by id argument", {
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "#asset:A",
      type = "Dataset",
      name = "A"
    )
  )

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "#asset:B",
      type = "Dataset",
      name = "B"
    )
  )

  res <- flatten_safe_data(roc, id = "#asset:A")

  expect_equal(nrow(res), 1)
  expect_equal(res$asset_id, "#asset:A")
})

test_that("flatten_safe_data.rocrate returns empty tibble on error", {
  bad_obj <- list() # not a rocrate

  res <- flatten_safe_data.rocrate(bad_obj)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

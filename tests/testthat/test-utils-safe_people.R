test_that("extract_safe_people.opal returns a valid RO-Crate with Person entities", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  # terminate connection when done with tests
  withr::defer(opalr::opal.logout(opal_con))

  roc <- extract_safe_people(opal_con)

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(roc))

  people <- rocrateR::get_entity(roc, type = "Person")

  expect_type(people, "list")
  expect_true(length(people) >= 1)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("extract_safe_people.opal updates an existing RO-Crate", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  # terminate connection when done with tests
  withr::defer(opalr::opal.logout(opal_con))

  base_roc <- rocrateR::rocrate_5s()

  roc <- extract_safe_people(opal_con, rocrate = base_roc)

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(roc))

  people <- rocrateR::get_entity(roc, type = "Person")
  expect_true(length(people) >= 1)

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("extract_safe_people.opal excludes admin and administrator accounts", {
  # the real OBiBa demo server doesn't set the `administrate` permission on
  # its `administrator` account, so this can't be exercised against it -
  # simulate a server where the exclusion actually has something to catch
  local_mocked_bindings(
    backend_users = function(x, ..., df = FALSE) {
      list(
        list(principal = "alice", groups = list("auditor")),
        list(principal = "bob_admin", groups = list("admin"))
      )
    },
    backend_sys_perms = function(x, ...) {
      tibble::tibble(
        subject = c("alice", "bob_admin"),
        permission = c("view", "administrate"),
        type = c("user", "user")
      )
    }
  )

  # records which users actually got passed into `safe_people()`, without
  # needing `<<-` - environments are mutable by reference
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
      calls$safe_people <- c(
        calls$safe_people,
        as.character(unlist(user, use.names = FALSE))
      )
      x
    }
  )

  con <- fake_opal_con()
  roc <- extract_safe_people(con)

  print(calls$safe_people)

  # `administrator` never reaches `safe_people()` because it's excluded
  # upstream, in `filter_safe_people()`, before the Person entity would
  # ever be created
  expect_false(any(tolower(calls$safe_people) %in% c("admin", "administrator")))
  expect_equal(calls$safe_people, "alice")
  expect_s3_class(roc, "rocrate")
})

test_that("extract_safe_people.opal iterates over all returned subject profiles", {
  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()
  # terminate connection when done with tests
  withr::defer(opalr::opal.logout(opal_con))

  users_raw <- backend_users(opal_con, df = FALSE)
  users_tbl <- dplyr::bind_rows(users_raw)

  expect_true(nrow(users_tbl) >= 1)

  roc <- extract_safe_people(opal_con)

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(roc))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("extract_safe_people.rocrate copies Person entities", {
  src <- rocrateR::rocrate_5s()

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#user1",
      type = "Person",
      name = "Alice"
    )
  )

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#user2",
      type = "Person",
      name = "Bob"
    )
  )

  new_roc <- extract_safe_people(src)

  # validate rocrate structure
  expect_no_error(rocrateR::is_rocrate(src))

  ents <- rocrateR::get_entity(new_roc, type = "Person")
  expect_equal(length(ents), 2)
})

test_that("extract_safe_people.rocrate filters Person entities by id", {
  src <- rocrateR::rocrate_5s()

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#keep",
      type = "Person",
      name = "Keep Me"
    )
  )

  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#drop",
      type = "Person",
      name = "Drop Me"
    )
  )

  new_roc <- extract_safe_people(src, id = "#keep")

  ents <- rocrateR::get_entity(new_roc, type = "Person")
  ids <- vapply(ents, function(e) getElement(e, "@id")[[1]], character(1))

  expect_equal(ids, "#keep")
})

test_that("extract_safe_people.rocrate infers ids from root Dataset author", {
  src <- rocrateR::rocrate_5s()

  # create person entity
  src <- rocrateR::add_entity(
    src,
    entity = rocrateR::entity(
      id = "#author1",
      type = "Person",
      name = "Author One"
    )
  )

  # attach author to root dataset
  src <- rocrateR::add_entity_value(
    src,
    id = "./",
    key = "author",
    value = list(list(`@id` = "#author1"))
  )

  new_roc <- extract_safe_people(src)

  ents <- rocrateR::get_entity(new_roc, type = "Person")
  expect_equal(length(ents), 1)
  expect_equal(getElement(ents[[1]], "@id"), "#author1")
})

test_that("extract_safe_people.rocrate errors when no matching Person entities found", {
  src <- rocrateR::rocrate_5s()

  # ignore warning about not finding People entities
  suppressWarnings(
    expect_error(
      extract_safe_people(src, id = "#nonexistent"),
      "No matching entities were found for the Author"
    )
  )
})

test_that("flatten_safe_people.default returns empty tibble", {
  res <- flatten_safe_people(list())

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("flatten_safe_people.rocrate extracts person metadata correctly", {
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "#p1",
      type = "Person",
      name = "Jane Doe",
      givenName = "Jane",
      familyName = "Doe",
      organisation = "Test Org"
    )
  )

  res <- flatten_safe_people(roc)

  expect_s3_class(res, "data.frame")
  expect_true(all(
    c("person_id", "name", "given_name", "family_name", "organisation") %in%
      names(res)
  ))
  expect_equal(res$person_id, "#p1")
  expect_equal(res$name, "Jane Doe")
})

test_that("flatten_safe_people.rocrate filters by id argument", {
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(id = "#p1", type = "Person", name = "A")
  )

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(id = "#p2", type = "Person", name = "B")
  )

  res <- flatten_safe_people(roc, id = "#p2")

  expect_equal(nrow(res), 1)
  expect_equal(res$person_id, "#p2")
})

test_that("flatten_safe_people.rocrate returns empty tibble on malformed input", {
  bad_obj <- list() # not a rocrate

  res <- flatten_safe_people.rocrate(bad_obj)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

# -------------------------------------------------------------------------
# S3 generic dispatch
# -------------------------------------------------------------------------
test_that("extract_safe_output dispatch works for rocrate", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- extract_safe_output(rocrate)

  expect_s3_class(out, "rocrate")
})

test_that("flatten_safe_output default returns empty tibble", {
  out <- flatten_safe_output(list())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

# -------------------------------------------------------------------------
# extract_safe_output.rocrate()
# -------------------------------------------------------------------------
test_that("extract_safe_output.rocrate errors when no File entities", {
  # create basic RO-Crate
  rocrate <- rocrateR::rocrate_5s()

  # ignore warning about not finding File entities
  suppressWarnings(
    expect_error(
      extract_safe_output(rocrate),
      "No matching entities were found!"
    )
  )
})

test_that("extract_safe_output.rocrate extracts all File entities", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- extract_safe_output(rocrate)

  expect_s3_class(out, "rocrate")

  files <- .get_entity(out, type = "File")
  expect_equal(length(files), 2)
})

test_that("extract_safe_output.rocrate filters by id correctly", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- extract_safe_output(rocrate, id = "outputs/user1/report.csv")

  files <- .get_entity(out, type = "File")
  ids <- vapply(files, function(x) getElement(x, "@id"), character(1))

  expect_true("outputs/user1/report.csv" %in% ids)
})

test_that("extract_safe_output.rocrate filters by user string in @id", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- extract_safe_output(rocrate, user = "user1")

  files <- .get_entity(out, type = "File")
  ids <- vapply(files, function(x) getElement(x, "@id"), character(1))

  expect_true(all(grepl("user1", ids)))
  expect_false(any(grepl("user2", ids)))
})

test_that("extract_safe_output.rocrate filters by both id and user", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- extract_safe_output(
    rocrate,
    id = "outputs/user1/report.csv",
    user = "user1"
  )

  files <- .get_entity(out, type = "File")
  expect_equal(length(files), 1)
  expect_equal(getElement(files[[1]], "@id"), "outputs/user1/report.csv")
})

test_that("extract_safe_output.rocrate errors when filters remove all entities", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  expect_error(
    extract_safe_output(rocrate, user = "nonexistent_user"),
    "No matching entities were found!"
  )
})

test_that("extract_safe_output.rocrate emits informative message", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  expect_message(
    extract_safe_output(rocrate, user = "user1"),
    "'File' entit"
  )
})

test_that("extract_safe_output.rocrate validates rocrate input", {
  expect_error(
    extract_safe_output(structure(list(), class = "rocrate"))
  )
})

# -------------------------------------------------------------------------
# flatten_safe_output.rocrate()
# -------------------------------------------------------------------------
test_that("flatten_safe_output.rocrate returns tibble with correct columns", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- flatten_safe_output(rocrate)

  expect_s3_class(out, "tbl_df")
  expect_true(all(
    c("id", "name", "description", "encodingFormat", "content") %in% names(out)
  ))
})

test_that("flatten_safe_output.rocrate extracts correct values", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- flatten_safe_output(rocrate)

  expect_true("report.csv" %in% out$name)
  expect_true("summary.txt" %in% out$name)
  expect_true("text/csv" %in% out$encodingFormat)
})

test_that("flatten_safe_output.rocrate filters by id", {
  # create an RO-Crate with File entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user1/report.csv",
        type = "File",
        name = "report.csv",
        description = "User 1 report",
        encodingFormat = "text/csv",
        content = "a,b\n1,2"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "outputs/user2/summary.txt",
        type = "File",
        name = "summary.txt",
        description = "User 2 summary",
        encodingFormat = "text/plain",
        content = "summary content"
      )
    )

  out <- flatten_safe_output(rocrate, id = "outputs/user2/summary.txt")

  expect_equal(nrow(out), 1)
  expect_equal(out$name, "summary.txt")
})

test_that("flatten_safe_output.rocrate returns empty tibble on error", {
  # broken object to trigger tryCatch
  bad_obj <- structure(list(), class = "rocrate")

  out <- flatten_safe_output(bad_obj)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

# -------------------------------------------------------------------------
# extract_safe_output.opal() (Integration style with safe mocking)
# -------------------------------------------------------------------------
test_that("extract_safe_output.opal calls safe_output and returns rocrate", {
  # dummy opal-like object
  opal_con <- structure(list(), class = "opal")

  # Mock safe_output to avoid real server dependency
  mock_safe_output <- function(x, ..., rocrate) {
    file <- rocrateR::entity(
      id = "outputs/mock/user/file.txt",
      type = "File",
      name = "file.txt"
    )
    rocrateR::add_entity(rocrate, file)
  }

  with_mocked_bindings(
    safe_output = mock_safe_output,
    {
      out <- extract_safe_output.opal(opal_con)
    }
  )

  expect_s3_class(out, "rocrate")

  files <- .get_entity(out, type = "File")
  expect_true(length(files) >= 1)
})

# -------------------------------------------------------------------------
# Optional TRUE Opal demo integration (dsROCrate audit realism)
# -------------------------------------------------------------------------

test_that("extract_safe_output.opal works with Opal demo server (if available)", {
  con <- try(opal_demo_con(), silent = TRUE)
  skip_if(inherits(con, "try-error"), "Opal demo server not available")

  # ignore warning about missing `path` to write outputs
  suppressWarnings(
    rocrate <- extract_safe_output(con, user = attr(con, "PEOPLE"))
  )

  expect_s3_class(rocrate, "rocrate")
})

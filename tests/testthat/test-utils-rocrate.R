test_that("CSV file content is loaded correctly", {
  skip_if_not_installed("rocrateR")

  # create temporary directory
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # create a CSV file
  csv_path <- file.path(tmp_dir, "data.csv")
  write.csv(data.frame(a = 1:3, b = letters[1:3]), csv_path, row.names = FALSE)

  # create minimal RO-Crate
  roc <- rocrateR::rocrate_5s()

  # Add File entity without content
  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "data.csv",
      type = "File",
      encodingFormat = "text/csv"
    )
  )

  # Run function
  roc2 <- load_content(roc, tmp_dir)

  # Retrieve entity
  ent <- rocrateR::get_entity(roc2, id = "data.csv")[[1]]

  expect_false(is.null(ent$content))
  expect_s3_class(ent$content[[1]], "data.frame")
  expect_equal(nrow(ent$content[[1]]), 3)
})

test_that("Text file content is loaded correctly", {
  skip_if_not_installed("rocrateR")

  # create temporary directory
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # create text file
  txt_path <- file.path(tmp_dir, "notes.txt")
  writeLines(c("line1", "line2"), txt_path)

  # create minimal RO-Crate
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "notes.txt",
      type = "File",
      encodingFormat = "text/plain"
    )
  )

  roc2 <- load_content(roc, tmp_dir)

  ent <- rocrateR::get_entity(roc2, id = "notes.txt")[[1]]

  expect_false(is.null(ent$content))
  expect_equal(ent$content[[1]], c("line1", "line2"))
})

test_that("Entities with existing content are not modified", {
  skip_if_not_installed("rocrateR")

  # create temporary directory
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # create minimal RO-Crate
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "existing.txt",
      type = "File",
      encodingFormat = "text/plain",
      content = list("already here")
    )
  )

  roc2 <- load_content(roc, tmp_dir)

  ent <- rocrateR::get_entity(roc2, id = "existing.txt")[[1]]

  expect_equal(ent$content[[1]], "already here")
})

test_that("Missing files do not break function and are skipped", {
  skip_if_not_installed("rocrateR")

  # create temporary directory
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # create minimal RO-Crate
  roc <- rocrateR::rocrate_5s()

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "missing.txt",
      type = "File",
      encodingFormat = "text/plain"
    )
  )

  expect_no_error({
    roc2 <- load_content(roc, tmp_dir)
  })

  ent <- rocrateR::get_entity(roc2, id = "missing.txt")[[1]]

  expect_true(is.null(ent$content))
})

test_that("load_content works end-to-end with real RO-Crate structure", {
  skip_if_not_installed("rocrateR")

  # create temporary directory
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # create files inside RO-Crate
  writeLines("hello world", file.path(tmp_dir, "file1.txt"))
  write.csv(
    data.frame(x = 42),
    file.path(tmp_dir, "file2.csv"),
    row.names = FALSE
  )

  # create minimal RO-Crate
  roc <- rocrateR::rocrate_5s()

  # add both files as entities
  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "file1.txt",
      type = "File",
      encodingFormat = "text/plain"
    )
  )

  roc <- rocrateR::add_entity(
    roc,
    entity = rocrateR::entity(
      id = "file2.csv",
      type = "File",
      encodingFormat = "text/csv"
    )
  )

  # run function
  roc2 <- load_content(roc, tmp_dir)

  ent1 <- rocrateR::get_entity(roc2, id = "file1.txt")[[1]]
  ent2 <- rocrateR::get_entity(roc2, id = "file2.csv")[[1]]

  expect_equal(ent1$content[[1]], "hello world")
  expect_s3_class(ent2$content[[1]], "data.frame")
  expect_equal(ent2$content[[1]]$x, 42)
})

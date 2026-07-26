test_that("All Rd files have a value section", {
  man_dir <- testthat::test_path("..", "man")
  db <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  # drop the package-level Rd (e.g., "dsROCrate-package")
  db <- db[!grepl("-package\\.Rd$", names(db))]

  has_value <- function(rd) {
    any(vapply(rd, function(x) attr(x, "Rd_tag") == "\\value", logical(1)))
  }

  results <- vapply(db, has_value, logical(1))
  missing <- names(db)[!results]

  expect_equal(
    length(missing),
    0L,
    info = paste("Missing \\value in:\n", paste(" -", missing, collapse = "\n"))
  )
})

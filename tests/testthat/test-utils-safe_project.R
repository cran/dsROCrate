# -------------------------------------------------------------------------
# S3 Generic Dispatch
# -------------------------------------------------------------------------

test_that("extract_safe_project dispatches correctly", {
  rocrate <- rocrateR::rocrate_5s()

  # ignore warning about lack of Project entity
  suppressWarnings(
    expect_error(
      extract_safe_project(rocrate),
      "No matching entities were found!"
    )
  )
})

test_that("flatten_safe_project dispatches default method", {
  out <- flatten_safe_project(list())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

# -------------------------------------------------------------------------
# extract_safe_project.rocrate()
# -------------------------------------------------------------------------

test_that("extract_safe_project.rocrate errors when no project entities", {
  rocrate <- rocrateR::rocrate_5s()

  # ignore warning about lack of Project entity
  suppressWarnings(
    expect_error(
      extract_safe_project(rocrate),
      "No matching entities were found!"
    )
  )
})

test_that("extract_safe_project.rocrate returns rocrate with project entities", {
  # create an RO-Crate with Project entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project2",
        type = "Project",
        name = "Project 2"
      )
    )

  out <- extract_safe_project(rocrate)

  expect_s3_class(out, "rocrate")

  projects <- rocrateR::get_entity(out, type = "Project")
  expect_true(length(projects) >= 1)
})

test_that("extract_safe_project.rocrate filters by id correctly", {
  # create an RO-Crate with Project entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project2",
        type = "Project",
        name = "Project 2"
      )
    )

  out <- extract_safe_project(rocrate, id = "#project1")

  projects <- rocrateR::get_entity(out, type = "Project")
  project_ids <- vapply(
    projects,
    function(x) getElement(x, "@id"),
    character(1)
  )

  expect_true("#project1" %in% project_ids)
})

test_that("extract_safe_project.rocrate throws error for non-matching id", {
  # create an RO-Crate with Project entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project2",
        type = "Project",
        name = "Project 2"
      )
    )

  expect_error(
    extract_safe_project(rocrate, id = "nonexistent"),
    "No matching entities were found!"
  )
})

test_that("extract_safe_project.rocrate emits informative message", {
  # create an RO-Crate with Project entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project2",
        type = "Project",
        name = "Project 2"
      )
    )

  expect_message(
    extract_safe_project(rocrate),
    "'Project' entit"
  )
})

test_that("extract_safe_project.rocrate adds linked dataset entities", {
  # create an RO-Crate with a Project and two Dataset entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1",
        hasPart = list(
          sapply(c("#ds1", "#ds2"), \(x) list(`id` = x))
        )
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds1",
        type = "Dataset",
        name = "Dataset 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds2",
        type = "Dataset",
        name = "Dataset 2"
      )
    )

  out <- extract_safe_project(rocrate)

  datasets <- rocrateR::get_entity(out, type = "Dataset")
  dataset_ids <- vapply(
    datasets,
    function(x) getElement(x, "@id"),
    character(1)
  )

  expect_true(all(c("#ds1", "#ds2") %in% dataset_ids))
})

# -------------------------------------------------------------------------
# flatten_safe_project.rocrate()
# -------------------------------------------------------------------------

test_that("flatten_safe_project.rocrate returns tibble", {
  # create an RO-Crate with a Project and two Dataset entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1",
        hasPart = list(
          sapply(c("#ds1", "#ds2"), \(x) list(`id` = x))
        )
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds1",
        type = "Dataset",
        name = "Dataset 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds2",
        type = "Dataset",
        name = "Dataset 2"
      )
    )

  # minimal stub for flatten_safe_data used internally
  flatten_safe_data <- function(x, ..., id = NULL) {
    tibble::tibble(
      asset_id = id,
      asset = c("table1", "table2")
    )
  }

  local_mocked_bindings(flatten_safe_data = flatten_safe_data)

  out <- flatten_safe_project(rocrate)

  expect_s3_class(out, "tbl_df")
  expect_true(all(
    c("project_id", "project", "asset_id", "asset") %in% names(out)
  ))
})

test_that("flatten_safe_project.rocrate extracts project metadata correctly", {
  # create an RO-Crate with a Project and two Dataset entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#project1",
        type = "Project",
        name = "Project 1",
        hasPart = list(
          sapply(c("#ds1", "#ds2"), \(x) list(`id` = x))
        )
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds1",
        type = "Dataset",
        name = "Dataset 1"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#ds2",
        type = "Dataset",
        name = "Dataset 2"
      )
    )

  flatten_safe_data <- function(x, ..., id = NULL) {
    tibble::tibble(
      asset_id = id,
      asset = paste0("name_", id)
    )
  }

  local_mocked_bindings(flatten_safe_data = flatten_safe_data)

  out <- flatten_safe_project(rocrate)

  expect_true("Project 1" %in% out$project)
  expect_true(any(grepl("name_", out$asset)))
})

test_that("flatten_safe_project.rocrate handles project without hasPart", {
  rocrate <- rocrateR::rocrate_5s()

  project <- rocrateR::entity(
    id = "project_no_part",
    type = "Project",
    name = "Lonely Project"
  )

  rocrate <- rocrate |>
    rocrateR::add_entity(project)

  out <- flatten_safe_project(rocrate)

  expect_equal(nrow(out), 1)
  expect_true(is.na(out$asset))
})

test_that("flatten_safe_project.rocrate returns empty tibble on error", {
  bad_obj <- structure(list(), class = "rocrate")

  out <- flatten_safe_project(bad_obj)

  expect_s3_class(out, "tbl_df")
})

# -------------------------------------------------------------------------
# extract_safe_project.opal() (INTEGRATION - demo server)
# -------------------------------------------------------------------------

test_that("extract_safe_project.opal works with demo Opal server", {
  # Skip if demo server is unreachable
  con <- try(opal_demo_con(), silent = TRUE)
  skip_if(inherits(con, "try-error"), "Opal demo server not available")

  rocrate <- extract_safe_project(con)

  expect_s3_class(rocrate, "rocrate")

  projects <- rocrateR::get_entity(rocrate, type = "Project")

  # CNSIM project should exist on demo server
  expect_true(length(projects) >= 1)
})

test_that("report.default errors", {
  expect_error(
    report(123),
    "Unknown class"
  )
})

test_that("report.character errors for invalid path", {
  expect_error(
    report("nonexistent_path", render = FALSE),
    "The provided path does not exist"
  )
})

test_that("report.character errors if read_rocrate fails", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  tmp_file <- tempfile(tmpdir = tmp_dir)
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))
  file.create(tmp_file)

  testthat::with_mocked_bindings(
    read_rocrate = function(...) stop("fail"),
    code = {
      expect_error(
        report(tmp_file, render = FALSE),
        "Could not determine how to load RO-Crate from provided input"
      )
    },
    .package = "rocrateR"
  )
})

test_that("report.character errors if input is invalid .zip", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".zip")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))
  file.create(tmp_file)

  expect_error(report(tmp_file, render = FALSE))
})

test_that("report.character dispatches to rocrate method", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))
  json_file <- file.path(tmp_dir, "ro-crate-metadata.json")
  writeLines("{}", json_file)

  fake_rocrate <- structure(list(), class = "rocrate")

  testthat::local_mocked_bindings(
    read_rocrate = function(...) fake_rocrate,
    .package = "rocrateR"
  )
  testthat::with_mocked_bindings(
    load_content = function(x, ...) x,
    report.character = function(x, ...) "OK",
    code = {
      result <- report(tmp_dir, render = FALSE)
      expect_equal(result, "OK")
    },
    .package = "dsROCrate"
  )
})

test_that("report.rocrate errors if required entities missing", {
  fake_rocrate <- structure(list(), class = "rocrate")

  testthat::local_mocked_bindings(
    is_rocrate = function(...) TRUE,
    .package = "rocrateR"
  )

  testthat::with_mocked_bindings(
    extract_safe_people = function(...) NULL,
    extract_safe_project = function(...) NULL,
    extract_safe_data = function(...) NULL,
    code = {
      expect_error(
        report(fake_rocrate, render = FALSE),
        "missing"
      )
    },
    .package = "dsROCrate"
  )
})

test_that("report.rocrate returns expected structure", {
  fake_rocrate <- structure(list(), class = "rocrate")

  safe_people <- list()
  safe_project <- list()
  safe_data <- list()

  testthat::local_mocked_bindings(
    is_rocrate = function(...) TRUE,
    .package = "rocrateR"
  )

  testthat::with_mocked_bindings(
    extract_safe_people = function(...) safe_people,
    extract_safe_project = function(...) safe_project,
    extract_safe_data = function(...) safe_data,
    extract_safe_setting = function(...) NULL,
    extract_safe_output = function(...) NULL,
    flatten_safe_people = function(...) {
      tibble::tibble(person_id = "u1", name = "dsuser", project = "P1")
    },
    flatten_safe_project = function(...) {
      tibble::tibble(
        project_id = "p1",
        project = "P1",
        asset_id = "t1",
        asset = "T1"
      )
    },
    flatten_safe_data = function(...) {
      tibble::tibble(asset_id = "t1", asset = "T1")
    },
    flatten_safe_output = function(...) tibble::tibble(),
    flatten_user_perm_entity = function(...) NULL,
    .overview_diagram = function(...) list(diag_lst = "diag", diag_path = NULL),
    .tidy_overview = function(...) tibble::tibble(project = "P1"),
    .markdown_report_header = function(...) "HEADER",
    .markdown_report_body = function(...) "BODY",
    .markdown_report_rocrate = function(...) "ROCRATE",
    code = {
      result <- report(
        fake_rocrate,
        render = FALSE,
        filepath = tempfile(fileext = ".md")
      )

      expect_type(result, "list")
      expect_named(
        result,
        c(
          "overview_diagram",
          "overview_data",
          "safe_people",
          "safe_project",
          "safe_data",
          "safe_data_permissions",
          "safe_setting",
          "safe_output"
        )
      )
    },
    .package = "dsROCrate"
  )
})

test_that("report.rocrate prevents overwrite", {
  fake_rocrate <- structure(list(), class = "rocrate")
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  tmp_file <- tempfile(tmpdir = tmp_dir, fileext = ".md")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))
  file.create(tmp_file)

  testthat::local_mocked_bindings(
    is_rocrate = function(...) TRUE,
    .package = "rocrateR"
  )

  testthat::with_mocked_bindings(
    extract_safe_people = function(...) list(),
    extract_safe_project = function(...) list(),
    extract_safe_data = function(...) list(),
    code = {
      expect_error(
        report(fake_rocrate, filepath = tmp_file, overwrite = FALSE),
        "existing file"
      )
    },
    .package = "dsROCrate"
  )
})

test_that("report.list aggregates multiple servers", {
  fake_rocrate <- structure(list(), class = "rocrate")

  server_output <- list(
    safe_people = tibble::tibble(id_person = "u1", name = "dsuser"),
    safe_project = tibble::tibble(
      id_project = "p1",
      project = "P1",
      asset_id = "t1",
      asset = "T1"
    ),
    safe_data = tibble::tibble(asset_id = "t1", asset = "T1"),
    safe_data_permissions = NULL,
    safe_setting = tibble::tibble(),
    safe_output = tibble::tibble(),
    overview_data = tibble::tibble(
      project = "P1",
      asset = "T1",
      person = "dsuser"
    )
  )

  testthat::local_mocked_bindings(
    is_rocrate = function(...) TRUE,
    .package = "rocrateR"
  )

  testthat::with_mocked_bindings(
    report = function(...) server_output,
    .overview_diagram = function(...) list(diag_lst = "diag", diag_path = NULL),
    .tidy_overview = function(...) tibble::tibble(Project = "P1"),
    .markdown_report_header = function(...) "HEADER",
    .markdown_report_body = function(...) "BODY",
    .markdown_report_rocrate = function(...) "ROCRATE",
    code = {
      result <- report(
        list(server1 = fake_rocrate, server2 = fake_rocrate),
        study_name = "StudyX",
        render = FALSE,
        filepath = tempfile(fileext = ".md")
      )

      expect_type(result, "list")
      expect_true("safe_people" %in% names(result))
    },
    .package = "dsROCrate"
  )
})

test_that(".tidy_overview collapses permissions correctly", {
  df <- tibble::tibble(
    project = "P1",
    asset = "T1",
    name = "dsuser",
    permission = c("read", "write")
  )

  result <- .tidy_overview(df, include_user_perm = TRUE)

  expect_true("Access Level" %in% colnames(result))
  expect_match(result$`Access Level`, "read")
})

test_that(".break_tibble splits by group", {
  df <- tibble::tibble(
    server = c("A", "B"),
    value = 1:2
  )

  result <- .break_tibble(df, "server")

  expect_length(result, 2)
})

test_that(".break_tibble returns input table when varname = NULL", {
  df <- tibble::tibble(
    server = c("A", "B"),
    value = 1:2
  )

  expect_equal(knitr::kable(df), .break_tibble(df, NULL))
})

test_that("report fails if the given path points to a non-existing directory", {
  expect_error(
    report(rocrateR::rocrate_5s(), filepath = "path/to/dir")
  )
})

test_that("report works end-to-end with real dsROCrate audit outputs", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  # generate audit RO-Crate for a Safe Project
  # suppress warnings about missing logs
  suppressWarnings({
    roc <- audit(
      opal_con,
      project = "CNSIM",
      path = tmp_dir
    )
  })

  out_file <- file.path(tmp_dir, "report.md")

  # ignore warnings about missing permission entities (e.g., @type = 'ControlAction')
  suppressWarnings(
    result <- report(
      roc,
      filepath = out_file,
      render = FALSE,
      overwrite = TRUE
    )
  )

  # ---- assertions ----
  expect_true(file.exists(out_file))
  expect_type(result, "list")

  expect_true("safe_people" %in% names(result))
  expect_true("safe_project" %in% names(result))
  expect_true("safe_data" %in% names(result))

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("report.character loads crate from disk", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  # generate audit RO-Crate for a Safe Project
  # suppress warnings about missing logs
  suppressWarnings({
    roc <- audit(
      opal_con,
      project = "CNSIM",
      path = tmp_dir
    )
  })

  # write crate to disk (real metadata file)
  path_to_rocrate_bag <- rocrateR::bag_rocrate(roc, path = tmp_dir)

  out_file <- file.path(tmp_dir, "report.md")

  # ignore warnings about missing permission entities (e.g., @type = 'ControlAction')
  suppressWarnings(
    result <- report(
      path_to_rocrate_bag,
      filepath = out_file,
      render = FALSE,
      overwrite = TRUE
    )
  )

  # ---- assertions ----
  expect_true(file.exists(out_file))
  expect_type(result, "list")

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("report.list aggregates outputs from a study audit", {
  # create temporary file
  tmp_dir <- file.path(tempdir(), "dsROCRate_tests")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE))

  # open connection to OBiBa's Opal demo server
  opal_con <- opal_demo_con()

  # generate audit RO-Crate for a study
  ## ignore warnings about missing logs
  suppressWarnings(
    roc <- audit(
      list(demo = opal_con),
      project = "CNSIM",
      path = tmp_dir
    )
  )

  out_file <- file.path(tmp_dir, "report.md")

  # ignore warnings about missing permission entities (e.g., @type = 'ControlAction')
  suppressWarnings(
    result <- report(
      roc,
      filepath = out_file,
      study_name = "StudyX",
      render = FALSE,
      overwrite = TRUE
    )
  )

  # ---- assertions ----
  expect_true(file.exists(out_file))
  expect_type(result, "list")

  # close connection to OBiBa's Opal demo server
  opalr::opal.logout(opal_con)
})

test_that("report.list handles missing outputs from a study audit", {
  testthat::with_mocked_bindings(
    report = function(...) list(),
    code = {
      result <- report(
        list(server1 = rocrateR::rocrate_5s()),
        study_name = "StudyX",
        render = FALSE,
        filepath = tempfile(fileext = ".md")
      )

      expect_type(result, "list")
    },
    .package = "dsROCrate"
  )
})

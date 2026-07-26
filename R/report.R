#' Create an RO-Crate report
#'
#' @param x This can be an RO-Crate ([rocrate][rocrateR::rocrate()] class) or a
#'     string with the path to an RO-Crate.
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::report`][report()].
#'
#' @returns RO-Crate report as markdown (.md) file and/or HTML.
#' @export
report <- function(x, ...) {
  UseMethod("report")
}

#' @rdname report
#' @export
report.ArmadilloCredentials <- function(x, ...) {
  stop(
    "The `report()` for the Armadillo backend is not currently implemented!",
    call. = FALSE
  )
}

#' @rdname report
#' @export
report.character <- function(
  x,
  ...,
  title = "DataSHIELD Report",
  filepath = tempfile(fileext = ".md"),
  render = TRUE,
  doc_format = "html",
  overwrite = FALSE,
  include_user_perm = TRUE,
  diag_title = "DataSHIELD server",
  diag_width = NULL,
  diag_height = NULL,
  max_line_length = 200
) {
  # attempt loading the RO-Crate
  rocrate <- rocrateR::load_rocrate(x)

  # call the next generic method
  rocrate |>
    report(
      title = title,
      filepath = filepath,
      render = render,
      doc_format = doc_format,
      overwrite = overwrite,
      include_user_perm = include_user_perm,
      diag_title = diag_title,
      diag_width = diag_width,
      diag_height = diag_height
    )
}

# @rdname report
#' @export
report.default <- function(x, ...) {
  stop(
    "Unknown class, please try either a file path or",
    " an object with `rocrate` class!"
  )
}

#' @param study_name String with the study name.
#' @rdname report
#' @export
report.list <- function(
  x,
  ...,
  study_name,
  title = "DataSHIELD Report",
  filepath = tempfile(fileext = ".md"),
  render = TRUE,
  doc_format = "html",
  overwrite = FALSE,
  include_user_perm = TRUE,
  diag_title = "DataSHIELD server",
  diag_width = NULL,
  diag_height = NULL,
  max_line_length = 200
) {
  # local bindings
  asset <- id <- name <- permission <- project <- server <- user <- NULL

  # validate that all the objects in the list, `x`, are valid RO-Crates
  sapply(x, rocrateR::is_rocrate)

  # generate individual reports for each RO-Crate
  report_outputs <- lapply(
    x,
    report,
    title = title,
    filepath = filepath,
    render = FALSE,
    doc_format = doc_format,
    overwrite = TRUE,
    include_user_perm = include_user_perm,
    diag_title = diag_title,
    diag_width = diag_width,
    diag_height = diag_height
  )

  # combine reports ----
  ## Safe People
  safe_people_all <- .extract_srvr_comp(report_outputs, "safe_people")
  ## Safe Data
  safe_data_all <- .extract_srvr_comp(report_outputs, "safe_data")
  ## Safe Projects
  safe_project_all <- .extract_srvr_comp(report_outputs, "safe_project")
  ## Safe Settings
  safe_setting_all <- .extract_srvr_comp(report_outputs, "safe_setting")
  ## Safe Outputs
  safe_output_all <- .extract_srvr_comp(report_outputs, "safe_output")
  ## Safe Data permissions (optional)
  safe_data_permissions_all <- report_outputs |>
    .extract_srvr_comp("safe_data_permissions")

  ## overview table ----
  overview_data_all <- tibble::tibble()
  ## combine aggregated data to generate new overview table
  safe_project_data_all <- safe_project_all #|>
  # dplyr::rename(asset = asset_name)

  # if data permissions were found, then combine with project-data details and
  # generate new overview table
  if (!is.null(safe_data_permissions_all)) {
    overview_data_all <- safe_data_permissions_all |>
      dplyr::left_join(safe_people_all, by = c("person_id", "server")) |>
      dplyr::left_join(safe_project_data_all, by = c("asset_id", "server")) |>
      dplyr::select(server, project, asset, permission, user = name)

    # if any Safe Outputs were found in the inputs, include in the overview
    if (!is.null(safe_output_all) && nrow(safe_output_all)) {
      overview_data_all <- overview_data_all |>
        dplyr::left_join(
          safe_output_all,
          by = c("server", "project", "asset", "user")
        )
    }
  } else {
    # extract previous overview table
    overview_data_all <- tryCatch(
      {
        report_outputs |>
          # extract each component per server
          lapply(getElement, name = "overview_data") |>
          # attach the server name as a new column
          purrr::imap(~ dplyr::mutate(.x, server = .y)) |>
          # combine rows
          dplyr::bind_rows()
      },
      error = function(e) {
        NULL
      }
    )
  }

  ## overview diagram ----
  overview_lst <- overview_data_all |>
    dplyr::rename(name = user) |>
    .overview_diagram(
      include_user_perm,
      filepath,
      render,
      diag_title,
      diag_width,
      diag_height
    )

  ## create tidy overview table ----
  tidy_overview_tbl <- overview_data_all |>
    dplyr::rename(name = user) |>
    .tidy_overview(include_user_perm = include_user_perm)

  ## create markdown report ----
  ## header and overview table
  report_contents <- c(
    .markdown_report_header(title, overview_data_all, overview_lst$diag_path),
    tidy_overview_tbl |>
      # # tidy up duplicated values in `project` and `asset`
      # dplyr::mutate(
      #   Project = unfill_vec(Project),
      #   Data = unfill_vec(Data)
      # ) |>
      dplyr::distinct() |>
      knitr::kable() |>
      paste0(collapse = "\n")
  )

  ## attach entities for 5 safes
  report_contents <- .markdown_report_body(
    report_contents = report_contents,
    overview_tbl = overview_data_all,
    safe_people_tbl = safe_people_all,
    safe_project_tbl = safe_project_all,
    safe_data_tbl = safe_data_all,
    safe_user_perm_tbl = safe_data_permissions_all,
    safe_setting_tbl = safe_setting_all,
    safe_output_tbl = safe_output_all,
    break_by = "server"
  )

  report_contents <- c(report_contents, "\n<hr />\n")

  ## append RO-Crates ----
  for (i in seq_along(x)) {
    # extract current RO-Crate
    rocrate <- x[i][[1]]
    # check if the input object has a `path` attribute
    if (!is.null(attr(rocrate, "path"))) {
      # check if any of the entities with `@type = 'File'` have empty `content`
      rocrate <- rocrate |>
        load_content(roc_path = attr(rocrate, "path"))
    }

    report_contents <- .markdown_report_rocrate(
      report_contents = report_contents,
      rocrate = rocrate,
      section_txt = ifelse(
        i == 1,
        paste0("## RO-Crates\n### '", names(x)[i], "' server"),
        paste0("### '", names(x)[i], "' server")
      ),
      max_line_length = max_line_length
    )
  }

  ## write contents inside file
  ### delete previous version
  if (overwrite) {
    unlink(filepath, recursive = TRUE, force = TRUE)
  }
  writeLines(report_contents, filepath)

  ## render document
  if (render) {
    if (tolower(doc_format) %in% c("html", "html_document")) {
      suppressWarnings({
        rmarkdown::render(
          filepath,
          "html_document",
          sub(".md$", ".html", filepath)
        )
        utils::browseURL(paste0("file://", sub(".md$", ".html", filepath)))
      })
    } else if (tolower(doc_format) %in% c("pdf", "pdf_document")) {
      suppressWarnings({
        rmarkdown::render(
          filepath,
          "pdf_document",
          sub(".md$", ".pdf", filepath)
        )
        utils::browseURL(paste0("file://", sub(".md$", ".pdf", filepath)))
      })
    } else {
      stop("The format `", doc_format, "` is not valid! Try 'html' or 'pdf'.")
    }
  } else {
    return(invisible(
      list(
        overview_diagram = overview_lst$diag_lst,
        overview_data = tidy_overview_tbl,
        safe_people = safe_people_all,
        safe_project_tbl = safe_project_all,
        safe_data = safe_data_all,
        safe_data_permissions = safe_data_permissions_all,
        safe_setting = safe_setting_all,
        safe_output = safe_output_all
      )
    ))
  }

  message("A report has been written to:\n ", filepath)

  # return list of data frames with Safe People, Projects, Data, etc.
  invisible(
    list(
      safe_people = safe_people_all,
      safe_project_tbl = safe_project_all,
      safe_data = safe_data_all,
      safe_data_permissions = safe_data_permissions_all,
      safe_setting = safe_setting_all,
      safe_output = safe_output_all
    )
  )
}

#' @param title String with title for the report (default: 'DataSHIELD Report').
#' @param filepath String with file path for Markdown report with the summary
#'     of the given object, `x`.
#' @param render Boolean flag to indicate whether to render the markdown report.
#' @param doc_format String with file format for the markdown report.
#' @param overwrite Boolean flag to indicate whether to overwrite a previous
#'     version of markdown report.
#' @param include_user_perm Boolean flag to indicate whether to include user
#'     permissions in the report overview's diagram.
#' @param diag_title String with title for the 'root' of the diagram (default:
#'     'DataSHIELD server').
#' @param diag_width Numeric value with width (in inches) for the report
#'     overview's diagram (default: `NULL`, estimated based on number of nodes).
#' @param diag_height Numeric value with height (in inches) for the report
#'     overview's diagram (default: `NULL`, estimated based on number of nodes).
#' @param max_line_length Integer with the maximum number of characters per line
#'     in the RO-Crate to be printed in the report.
#' @rdname report
#' @export
report.rocrate <- function(
  x,
  ...,
  title = "DataSHIELD Report",
  filepath = tempfile(fileext = ".md"),
  render = TRUE,
  doc_format = "html",
  overwrite = FALSE,
  include_user_perm = TRUE,
  diag_title = "DataSHIELD server",
  diag_width = NULL,
  diag_height = NULL,
  max_line_length = 200
) {
  # local bindings ----
  actionStatus <- asset <- asset_id <- description <- fx <- NULL
  encodingFormat <- id <- name <- permission <- perm_id <- project <- NULL
  table_name <- timestamp <- type <- person_id <- user <- NULL

  # validate RO-Crate ----
  rocrateR::is_rocrate(x)

  # ensure the given file path exists
  if (!dir.exists(dirname(filepath))) {
    stop(
      "The given file path directory does not exist. Try running the following",
      "command first:\n\t`mkdir -r",
      dirname(filepath),
      "`",
      call. = FALSE
    )
  }

  # check if the filepath points to an existing file
  if (file.exists(filepath)) {
    if (!overwrite) {
      stop(
        "The given file path points to an existing file. Try setting ",
        "`overwrite = TRUE` or changing the file path!",
        call. = FALSE
      )
    }
  }

  # check if the input object has a `path` attribute
  if (!is.null(attr(x, "path"))) {
    # check if any of the entities with `@type = 'File'` have empty `content`
    x <- x |>
      load_content(roc_path = attr(x, "path"))
  }

  # pre-processing ----
  ## Safe People ----
  # attempt to extract Safe People details
  safe_people_rocrate <- tryCatch(
    {
      extract_safe_people(x)
    },
    error = function(e) {
      NULL
    }
  )

  ## Safe Data ----
  # attempt to extract Safe Data details
  safe_data_rocrate <- tryCatch(
    {
      extract_safe_data(x)
    },
    error = function(e) {
      NULL
    }
  )

  # attempt to extract user permission entities
  user_perm_entity_lst <- tryCatch(
    {
      .get_entity(
        x,
        type = c("ReadAction", "WriteAction", "ControlAction")
      )
    },
    error = function(e) {
      NULL
    }
  )

  ## Safe Projects ----
  # extract project IDs from the Safe People RO-Crate
  member_of <- tryCatch(
    {
      safe_people_rocrate |>
        .get_entity(type = "Person") |>
        sapply(\(x) getElement(x, "memberOf")) |>
        unlist()
    },
    error = function(e) {
      NULL
    }
  )

  # attempt to extract Safe Project details
  safe_project_rocrate <- tryCatch(
    {
      extract_safe_project(x, id = member_of)
    },
    error = function(e) {
      NULL
    }
  )

  ## Safe Settings ----
  # attempt to extract Safe Setting details
  safe_setting_rocrate <- tryCatch(
    {
      extract_safe_setting(x)
    },
    error = function(e) {
      NULL
    }
  )

  ## Safe Outputs ----
  # attempt to extract Safe Output details
  safe_outputs_rocrate <- tryCatch(
    {
      extract_safe_output(x)
    },
    error = function(e) {
      NULL
    }
  )

  # verify contents of RO-Crate ----
  has_project_ents <- !is.null(safe_project_rocrate)
  has_data_ents <- !is.null(safe_data_rocrate)
  has_people_ents <- !is.null(safe_people_rocrate)

  # if not all these flags are `TRUE`, return error message
  if (!all(has_project_ents, has_data_ents, has_people_ents)) {
    stop(
      paste0(
        "The given RO-Crate is missing the following:",
        ifelse(has_project_ents, "", " - Project entity (`@type = 'Project'`)"),
        ifelse(has_data_ents, "", " - Data entity (`@type = 'Dataset'`)"),
        ifelse(has_people_ents, "", " - People entity (`type = 'Person'`)"),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  # create overview ----
  overview_tbl <- tibble::tibble()
  ### extract (if available) table with user permissions
  user_perm_tbl <- flatten_user_perm_entity(user_perm_entity_lst)
  ### extract table with Safe People details
  safe_people_tbl <- flatten_safe_people(safe_people_rocrate)
  ### extract table with Safe Project details
  safe_project_tbl <- flatten_safe_project(safe_project_rocrate)
  ### extract table with Safe Data details
  safe_data_tbl <- flatten_safe_data(safe_data_rocrate)
  if (!is.null(user_perm_tbl) && nrow(user_perm_tbl) > 0) {
    overview_tbl <- user_perm_tbl |>
      # combine with Safe People details
      dplyr::left_join(safe_people_tbl, by = "person_id") |>
      # subset columns of interest
      dplyr::select(perm_id, person_id, name, asset_id, permission) |>
      # combine with Safe Data details
      dplyr::left_join(safe_data_tbl, by = "asset_id") |>
      # combine with Safe Project details
      dplyr::left_join(safe_project_tbl, by = c("asset_id", "asset")) #|>
    # # drop unused columns
    # dplyr::select(-id, -person_id, -asset_id, -type) |>
    # dplyr::rename(asset = asset_name)
  } else {
    overview_tbl <- flatten_safe_people(safe_people_rocrate) |>
      purrr::pmap(function(person_id, name, ...) {
        tibble::tibble(person_id, name) |>
          dplyr::bind_cols(flatten_safe_project(safe_project_rocrate))
      }) |>
      purrr::list_c()
  }

  # attempt extracting list of functions executed by the users from Safe Outputs
  safe_output_tbl <- flatten_safe_output(safe_outputs_rocrate) |>
    # extract only entities with mappings and functions, stored in CSV format
    dplyr::filter(encodingFormat == "text/csv") |>
    # extract content
    purrr::pluck("content", .default = list()) |>
    purrr::list_flatten() |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind()

  # initialise safe_output_tbl_v2, to be included in the returned outputs
  safe_output_tbl_v2 <- tibble::tibble()

  if (!is.null(safe_output_tbl) && nrow(safe_output_tbl) > 0) {
    # split `asset` into `project` and `asset`
    safe_output_tbl_v2 <- safe_output_tbl |>
      dplyr::mutate(
        project = gsub("(?=\\.).*$", "", asset, perl = TRUE),
        asset = gsub("^.*(?<=\\.)", "", asset, perl = TRUE)
      ) |>
      dplyr::distinct(project, asset, user, fx, timestamp)

    # append the list of functions to the overview table
    overview_tbl <- overview_tbl |>
      dplyr::left_join(
        safe_output_tbl_v2,
        by = c("project" = "project", "asset" = "asset", "name" = "user")
      ) |>
      # replace 'NA' in fx & timestamp with empty string
      dplyr::mutate(
        timestamp = dplyr::case_when(is.na(timestamp) ~ "", TRUE ~ timestamp),
        fx = dplyr::case_when(is.na(fx) ~ "", TRUE ~ fx)
      )
  }

  # overview diagram ----
  overview_lst <- .overview_diagram(
    overview_tbl,
    include_user_perm,
    filepath,
    render,
    diag_title,
    diag_width,
    diag_height
  )

  # create tidy overview table ----
  tidy_overview_tbl <- .tidy_overview(overview_tbl, include_user_perm)

  # create markdown report ----
  ## header and overview table
  report_contents <- c(
    .markdown_report_header(title, overview_tbl, overview_lst$diag_path),
    tidy_overview_tbl |>
      # # tidy up duplicated values in `project` and `asset`
      # dplyr::mutate(
      #   Project = unfill_vec(Project),
      #   Data = unfill_vec(Data)
      # ) |>
      dplyr::distinct() |>
      knitr::kable() |>
      paste0(collapse = "\n")
  )

  ## attach entities for 5 safes
  report_contents <- .markdown_report_body(
    report_contents = report_contents,
    overview_tbl = overview_tbl,
    safe_people_tbl = flatten_safe_people(safe_people_rocrate),
    safe_project_tbl = flatten_safe_project(safe_project_rocrate),
    safe_data_tbl = flatten_safe_data(safe_data_rocrate),
    safe_user_perm_tbl = flatten_user_perm_entity(user_perm_entity_lst),
    safe_setting_tbl = flatten_safe_setting(safe_setting_rocrate),
    safe_output_tbl = safe_output_tbl_v2
  )

  report_contents <- c(report_contents, "\n<hr />\n")

  # append input RO-Crate ----
  report_contents <- .markdown_report_rocrate(
    report_contents = report_contents,
    rocrate = x,
    section_txt = "## RO-Crate",
    max_line_length = max_line_length
  )

  ## write contents inside file
  ### delete previous version
  if (overwrite) {
    unlink(filepath, recursive = TRUE, force = TRUE)
  }
  writeLines(report_contents, filepath)

  ## render document
  if (render) {
    if (tolower(doc_format) %in% c("html", "html_document")) {
      suppressWarnings({
        rmarkdown::render(
          filepath,
          "html_document",
          sub(".md$", ".html", filepath)
        )
        utils::browseURL(paste0("file://", sub(".md$", ".html", filepath)))
      })
    } else if (tolower(doc_format) %in% c("pdf", "pdf_document")) {
      suppressWarnings({
        rmarkdown::render(
          filepath,
          "pdf_document",
          sub(".md$", ".pdf", filepath)
        )
        utils::browseURL(paste0("file://", sub(".md$", ".pdf", filepath)))
      })
    } else {
      stop("The format `", doc_format, "` is not valid! Try 'html' or 'pdf'.")
    }
  } else {
    return(invisible(
      list(
        overview_diagram = overview_lst$diag_lst,
        overview_data = tidy_overview_tbl,
        safe_people = flatten_safe_people(safe_people_rocrate),
        safe_project = flatten_safe_project(safe_project_rocrate),
        safe_data = flatten_safe_data(safe_data_rocrate),
        safe_data_permissions = flatten_user_perm_entity(user_perm_entity_lst),
        safe_setting = flatten_safe_setting(safe_setting_rocrate),
        safe_output = safe_output_tbl_v2
      )
    ))
  }

  message("A report has been written to:\n ", filepath)

  # return list of data frames with Safe People, Projects, Data, etc.
  invisible(
    list(
      safe_people = flatten_safe_people(safe_people_rocrate),
      safe_project = flatten_safe_project(safe_project_rocrate),
      safe_data = flatten_safe_data(safe_data_rocrate),
      safe_data_permissions = flatten_user_perm_entity(user_perm_entity_lst),
      safe_setting = flatten_safe_setting(safe_setting_rocrate),
      safe_output = safe_output_tbl_v2
    )
  )
}

.extract_srvr_comp <- function(x, component) {
  tryCatch(
    {
      x |>
        # extract each component per server
        purrr::map(component) |>
        # lapply(getElement, name = component) |>
        # filter out NULL elements
        purrr::keep(\(x) !is.null(x)) |>
        # attach the server name as a new column
        purrr::imap(~ dplyr::mutate(.x, server = .y)) |>
        # combine rows
        dplyr::bind_rows()
    },
    error = function(e) {
      NULL
    }
  )
}

#' Create diagram for RO-Crate overview
#'
#' @param overview_tbl Data frame with overview details for the RO-Crate.
#' @inheritParams report
#'
#' @returns Diagram object
#' @keywords internal
#' @noRd
.overview_diagram <- function(
  overview_tbl,
  include_user_perm,
  filepath,
  render,
  diag_title,
  diag_width,
  diag_height
) {
  # local bindings
  fx <- name <- permission <- project <- asset <- NULL

  ## initialise `vars` and `labelvar`
  vars <- c("project", "asset")
  labelvar <- c(project = "Project", asset = "Data")

  # check if `overview_tbl` has `permission` field AND include_user_perm = TRUE
  if ("permission" %in% colnames(overview_tbl) && include_user_perm) {
    vars <- c(vars, "permission")
    labelvar <- c(labelvar, permission = "Access Level")
  }

  # attach name/user and label it 'People'
  vars <- c(vars, "name")
  labelvar <- c(labelvar, name = "People")

  # check if `overview_tbl` has a `fx` field
  if ("fx" %in% colnames(overview_tbl)) {
    vars <- c(vars, "fx")
    labelvar <- c(labelvar, fx = "Function")

    # replace 'NA' with empty string for `fx`
    overview_tbl <- overview_tbl |>
      dplyr::mutate(fx = ifelse(is.na(fx), "", fx))
  }

  # attach 'server' if found in `overview_tbl`
  if ("server" %in% colnames(overview_tbl)) {
    vars <- c("server", vars)
    labelvar <- c(server = "Server", labelvar)
  }

  # aggregate the data if `permission` exists in `overview_tbl`
  if ("permission" %in% colnames(overview_tbl)) {
    overview_agg <- overview_tbl |>
      dplyr::group_by(dplyr::pick(vars[vars != "permission"])) |>
      dplyr::reframe(
        permission = paste0(unique(permission), collapse = " & "),
      )
  } else {
    overview_agg <- overview_tbl
  }

  ## generate diagram
  diagram_lst <- overview_agg |>
    vtree::vtree(
      vars = vars,
      labelvar = labelvar,
      showpct = FALSE,
      showcount = FALSE,
      horiz = FALSE,
      varnamebold = TRUE,
      splitwidth = 1,
      vsplitwidth = 1,
      folder = dirname(filepath),
      title = diag_title,
      # imageFileOnly = render,
      pngknit = render,
      # pxheight = min(80 * nrow(overview_tbl), 500),
      # pxwidth = 200 * nrow(overview_tbl),
      prune = list(fx = "")
    )

  ## if `render = TRUE`, then render diagram as PNG
  diagram_filepath <- NULL
  ### estimate number of nodes
  dot <- diagram_lst$x$diagram
  nodes <- dot |>
    gregexpr(pattern = "Node_[A-Za-z0-9_]+", perl = TRUE) |>
    (\(.) regmatches(dot, .))() |>
    unlist() |>
    unique()
  num_nodes <- length(nodes)
  ### scale width/height based on nodes
  scale_factor <- 0.5 # inches per node
  if (is.null(diag_width)) {
    width <- max(12, num_nodes * scale_factor)
  } else {
    width <- diag_width
  }
  if (is.null(diag_height)) {
    height <- max(6, num_nodes * scale_factor)
  } else {
    height <- diag_height
  }
  if (render) {
    diagram_filepath <- diagram_lst |>
      vtree::grVizToImageFile(
        folder = dirname(filepath),
        filename = gsub("md$", "png", basename(filepath))
      )
  }

  # # find path to latest PNG generated with `vtree`
  # diagram_filepath <- list.files(dirname(filepath), "^vtree")
  # diagram_filepath <- diagram_filepath[length(diagram_filepath)]

  return(list(diag_lst = diagram_lst, diag_path = diagram_filepath))
}

#' Create tidy version of the overview table
#'
#' @inheritParams .overview_diagram
#'
#' @returns Data frame with tidy overview table.
#' @keywords internal
#' @noRd
.tidy_overview <- function(overview_tbl, include_user_perm) {
  # local bindings
  fx <- permission <- timestamp <- NULL

  ## initialise `vars` and `varslab`
  vars <- c("project", "asset")
  varslab <- c("Project" = "project", "Data" = "asset")

  # check if `overview_tbl` has `permission` field AND include_user_perm = TRUE
  if ("permission" %in% colnames(overview_tbl) && include_user_perm) {
    vars <- c(vars, "permission")
    varslab <- c(varslab, "Access Level" = "permission")
  }

  # attach name/user and label it 'People'
  vars <- c(vars, "name")
  varslab <- c(varslab, "People" = "name")

  # check if `overview_tbl` has a `fx` field
  if ("fx" %in% colnames(overview_tbl)) {
    vars <- c(vars, "fx", "timestamp")
    varslab <- c(
      varslab,
      "Function" = "fx",
      "Timestamp" = "timestamp"
    )

    # replace 'NA' with empty string for `fx` and `timestamp`
    overview_tbl <- overview_tbl |>
      dplyr::mutate(
        fx = ifelse(is.na(fx), "", fx),
        timestamp = ifelse(is.na(timestamp), "", timestamp)
      )
  }

  # attach 'server' if found in `overview_tbl`
  if ("server" %in% colnames(overview_tbl)) {
    vars <- c("server", vars)
    varslab <- c("Server" = "server", varslab)
  }

  # create tidy data frame
  tidy_overview_tbl <- overview_tbl |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(vars)))

  if ("permission" %in% colnames(overview_tbl)) {
    if ("fx" %in% colnames(overview_tbl)) {
      vars_agg <- vars[!vars %in% c("permission", "fx", "timestamp")]
      tidy_overview_tbl <- tidy_overview_tbl |>
        dplyr::group_by(dplyr::pick(vars_agg)) |>
        dplyr::reframe(
          permission = paste0(unique(permission), collapse = " & "),
          fx = fx,
          timestamp = timestamp,
          .groups = "drop"
        )
    } else {
      tidy_overview_tbl <- tidy_overview_tbl |>
        dplyr::group_by(dplyr::pick(vars[vars != "permission"])) |>
        dplyr::reframe(
          permission = paste0(unique(permission), collapse = " & ")
        )
    }
  }

  tidy_overview_tbl <- tidy_overview_tbl |>
    dplyr::select(dplyr::all_of(varslab)) |>
    dplyr::distinct()

  return(tidy_overview_tbl)
}


#' Generate Markdown report's header
#'
#' @inheritParams .overview_diagram
#' @inheritParams report
#'
#' @returns String with report's header
#' @keywords internal
#' @noRd
.markdown_report_header <- function(title, overview_tbl, diagram_filepath) {
  # initialise variables for the report header
  unique_users_vct <- unique(c(
    getElement(overview_tbl, "name"),
    getElement(overview_tbl, "user")
  ))
  unique_project_vct <- unique(getElement(overview_tbl, "project"))
  unique_servers_vct <- unique(getElement(overview_tbl, "server"))
  ## initialise markdown header (see https://rmarkdown.rstudio.com/lesson-9.html)
  paste0(
    "---\n",
    paste0("title: ", title),
    "\noutput:\n",
    "  pdf_document:\n",
    "    df_print: kable\n",
    "    highlight: tango\n",
    "  html_document: default\n",
    "fontsize: 11pt\n",
    "geometry:\n",
    "  - margin=.5in\n",
    "  - landscape\n",
    "header-includes: \n",
    "  - \\usepackage{array}\n",
    "  - \\usepackage{longtable}\n",
    "  - \\usepackage{xurl}\n",
    "  - \\usepackage{hyphenat}\n",
    "  - \\usepackage{microtype}\n",
    "  - \\sloppy\n",
    "  - \\setlength{\\emergencystretch}{3em}\n",
    "  - \\usepackage{float}\n",
    "  - \\usepackage{titling}\n",
    "  - \\setlength{\\droptitle}{-1.5cm}\n",
    "  - \\usepackage[breakable]{tcolorbox}\n",
    "  - \\usepackage{tcolorbox}\n",
    "  - \\tcbuselibrary{breakable}\n",
    "  - \\renewenvironment{Shaded}{\\begin{tcolorbox}[breakable,boxrule=0pt,",
    "colback=gray!10]}{\\end{tcolorbox}}\n",
    paste0("date: ", format(Sys.time(), '%Y-%m-%d %H:%M:%S')),
    "\n---\n",
    paste0(
      "This report contains details for ",
      length(unique_users_vct),
      " user",
      ifelse(length(unique_users_vct) > 1, "s", ""),
      " and ",
      length(unique_project_vct),
      " project",
      ifelse(length(unique_project_vct) > 1, "s", ""),
      ". In addition, the tables they have access to within",
      ifelse(length(unique_project_vct) > 1, " a ", " the "),
      "project. \n",
      ifelse(
        "server" %in% colnames(overview_tbl),
        paste0(
          "\n**Note:** the data shown in this report was extracted from ",
          length(unique_servers_vct),
          " server",
          ifelse(length(unique_servers_vct) > 1, "s", ""),
          "."
        ),
        ""
      ),
      "\n\n"
    ),
    "## Overview\n\n",
    "<div style=\"margin:0;\">\n",
    # "::: {style=\"text-align: center;\"}\n",
    "<!-- PDF-only -->\n",
    "```{=latex}\n",
    paste0(
      "\\begin{figure}[H]\n",
      "\\centering\n",
      "\\includegraphics[height=0.75\\textheight, keepaspectratio, width=0.9\\textwidth]{",
      diagram_filepath,
      "}",
      "\\caption{RO-Crate Overview}\n",
      "\\end{figure}\n```\n"
    ),
    "<!-- HTML-only -->",
    "```{=html}\n",
    paste0(
      "<img src=\"",
      diagram_filepath,
      "\" alt=\"RO-Crate Overview\" ",
      "style=\"display:block; margin-left:auto; margin-right:auto;\" />\n"
    ),
    "```\n</div>\n\n\\newpage"
  )
}

#' Generate Markdown report's body
#'
#' @inheritParams .overview_diagram
#' @param report_contents String with Markdown report (e.g., header).
#' @param safe_people_tbl Data frame with Safe People details.
#' @param safe_project_tbl Data frame with Safe Project details.
#' @param safe_data_tbl Data frame with Safe Data details.
#' @param safe_user_perm_tbl Data frame with Safe Data user permissions.
#' @param safe_setting_tbl Data frame with Safe Setting details.
#' @param safe_output_tbl Data frame with Safe Output details.
#' @param break_by Optional string with variable to be used for breaking down
#'     each table (e.g., server), as opposed to display all the results in a
#'     single table.
#'
#' @returns String with updated Markdown report.
#' @keywords internal
#' @noRd
.markdown_report_body <- function(
  report_contents,
  overview_tbl,
  safe_people_tbl,
  safe_project_tbl,
  safe_data_tbl,
  safe_user_perm_tbl,
  safe_setting_tbl,
  safe_output_tbl,
  break_by = NULL
) {
  # local bindings
  actionStatus <- description <- permission <- NULL

  report_contents <- c(report_contents, "\n<hr />\n", "## Entities")

  ## append Safe People details
  report_contents <- c(
    report_contents,
    "\n### People\n",
    safe_people_tbl |>
      .break_tibble(varname = break_by) |>
      paste0(collapse = "\n")
  )

  ## append Safe Project & Safe Data details
  report_contents <- c(
    report_contents,
    "### Project(s)\n",
    safe_project_tbl |>
      .break_tibble(varname = break_by) |>
      paste0(collapse = "\n")
  )

  ## append Safe Data details
  report_contents <- c(
    report_contents,
    "### Data\n",
    safe_data_tbl |>
      .break_tibble(varname = break_by) |>
      paste0(collapse = "\n")
  )

  ## append Safe Data permissions
  #### check if `overview_tbl` has `permission` field
  if ("permission" %in% colnames(overview_tbl)) {
    report_contents <- c(
      report_contents,
      "#### Data permissions\n",
      safe_user_perm_tbl |>
        dplyr::select(-actionStatus, -description, -permission) |>
        .break_tibble(varname = break_by) |>
        paste0(collapse = "\n")
    )
  }

  ## append Safe Settings details
  report_contents <- c(
    report_contents,
    "### Settings\n",
    safe_setting_tbl |>
      .break_tibble(varname = break_by) |>
      paste0(collapse = "\n")
  )

  if (!is.null(safe_output_tbl) && nrow(safe_output_tbl) > 0) {
    ## append Safe Outputs details
    report_contents <- c(
      report_contents,
      "### Outputs\n",
      safe_output_tbl |>
        .break_tibble(varname = break_by) |>
        paste0(collapse = "\n")
    )
  }

  return(report_contents)
}

#' Embed RO-Crate in Markdown report
#'
#' @inheritParams .markdown_report_body
#' @param rocrate RO-Crate object (see [rocrateR::rocrate]).
#' @param section_txt String with to be used as the section header
#'     (e.g., RO-Crate).
#'
#' @returns String with update Markdown report.
#' @keywords internal
#' @noRd
.markdown_report_rocrate <- function(
  report_contents,
  rocrate,
  section_txt,
  max_line_length = 200
) {
  # save the input into intermediate JSON file
  tmp_file <- tempfile(fileext = ".json")
  # delete temporary file
  on.exit(unlink(tmp_file, recursive = TRUE, force = TRUE))
  # store RO-Crate in JSON format
  jsonlite::write_json(
    rocrate,
    path = tmp_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  # load formatted RO-Crate as text
  rocrate_txt <- readLines(tmp_file) |>
    # shorten long lines, beyond `max_line_length`
    sapply(function(x) {
      if (nchar(x) > max_line_length && max_line_length > 10) {
        return(paste0(
          substr(x, 1, max_line_length - 10),
          "... <line truncated> ...",
          substr(x, nchar(x) - 10, nchar(x))
        ))
      }
      return(x)
    })
  report_contents <- c(
    report_contents,
    "\n\\newpage\n",
    section_txt,
    "\n```json",
    # display formatted RO-Crate
    rocrate_txt,
    "\n```"
  )
}

#' Break tibble by group, `varname`
#'
#' @param df Data frame to be broken down into groups.
#' @param varname String with variable name
#'
#' @returns String with data frame rendered with `kable`.
#' @keywords internal
#' @noRd
.break_tibble <- function(df, varname) {
  # local bindings
  temp <- NULL

  # if `varname` is NULL or not a column in `df`, render all the data
  if (is.null(varname) || !(varname %in% colnames(df))) {
    return(knitr::kable(df))
  }

  # extract unique values for `varname` in `df`
  unique_vals <- unique(getElement(df, varname))

  # render data frame based on groups
  sapply(unique_vals, function(v) {
    c(
      paste0("\n#### '", v, "' ", varname, "\n"),
      df |>
        dplyr::rename(temp = !!varname) |>
        dplyr::filter(temp == v) |>
        dplyr::select(-temp) |>
        # drop out empty columns, equivalent to `janitor::remove_empty`
        Filter(f = function(x) any(!is.na(x))) |>
        knitr::kable()
    ) |>
      paste0(collapse = "\n")
  })
}

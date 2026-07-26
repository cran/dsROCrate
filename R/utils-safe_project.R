#' Extract Safe Project entity(ies)
#'
#' @inheritParams safe_data
#' @param ... Other optional arguments. See the full documentation
#' @param rocrate (Optional) RO-Crate object to update with Safe Project
#'     details.
#' @param id (Optional) Vector with `@id` strings for Safe Project entity(ies)
#'     to be extracted from the given RO-Crate, `x`.
#'
#' @returns List with Safe Project entity(ies).
#' @keywords internal
#' @noRd
extract_safe_project <- function(x, ...) {
  UseMethod("extract_safe_project")
}

#' @export
extract_safe_project.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s()
) {
  # extract list with all projects
  ds <- backend_projects(x)

  # cycle through the data source (x) and extract project details
  for (i in seq_len(nrow(ds))) {
    project <- ds[i, "name"]
    if (!is.na(project) && !is.null(project)) {
      suppressWarnings({
        rocrate <- rocrate |>
          safe_project(project = project, connection = x)
      })
    }
  }

  # return RO-Crate with Safe Project details
  return(rocrate)
}

#' @export
extract_safe_project.rocrate <- function(
  x,
  ...,
  id = NULL,
  rocrate = rocrateR::rocrate_5s()
) {
  # validate RO-Crate
  rocrateR::is_rocrate(x)

  # extract Project entities
  entities_lst <- .get_entity(x, type = "Project")

  # extract Dataset entities
  data_entities_lst <- .get_entity(x, type = "Dataset")

  # if `id` was provided, then filter out only those entities
  if (!is.null(id)) {
    idx <- entities_lst |>
      sapply(\(x) getElement(x, "@id") %in% id)
    entities_lst <- entities_lst[idx]
  }

  # check if any entities were found
  if (length(entities_lst) == 0) {
    stop("No matching entities were found!", call. = FALSE)
  } else {
    message(
      length(entities_lst),
      " 'Project' entit",
      ifelse(length(entities_lst) == 1, "y was", "ies were"),
      " found!"
    )
  }

  # add entities to the RO-Crate
  suppressWarnings({
    for (p in seq_along(entities_lst)) {
      # attempt extracting Safe Data entities
      has_part <- getElement(entities_lst[[p]], "hasPart") |>
        unlist()
      # keep only entities linked to the current project
      idx <- data_entities_lst |>
        sapply(\(x) getElement(x, "@id") %in% has_part)
      proj_data_ents_lst <- data_entities_lst[idx]
      if (length(proj_data_ents_lst) > 0) {
        rocrate <- rocrate |>
          rocrateR::add_entity(proj_data_ents_lst, overwrite = TRUE)
      }
      # add current project entity
      rocrate <- rocrate |>
        rocrateR::add_entity(entities_lst[[p]])
    }
  })

  # return RO-Crate with the Safe Data details
  return(rocrate)
}

#' Flatten object with Safe Project details
#'
#' @param x Object (e.g., RO-Crate) with Safe Project details. This
#'     can be generated with the `extract_safe_project()` function.
#' @param ... Other optional arguments (not in used).
#' @param y Object (e.g., RO-Crate) with Safe Data details. This can be
#'     generated with the `extract_safe_data()` function. If not provided, it
#'     uses the `x` by default.
#'
#' @returns Data frame with safe project details.
#' @keywords internal
#' @noRd
flatten_safe_project <- function(x, ...) {
  UseMethod("flatten_safe_project")
}

#' @export
flatten_safe_project.default <- function(x, ...) {
  return(tibble::tibble())
}

#' @export
flatten_safe_project.rocrate <- function(x, ..., y = x) {
  tryCatch(
    {
      x |>
        # extract project entities
        .get_entity(type = "Project") |>
        # extract datasets/tables associated with the project
        lapply(function(ent) {
          # extract IDs for datasets
          has_part <- getElement(ent, "hasPart") |>
            unlist()
          project_id <- getElement(ent, "@id")
          project <- getElement(ent, "name")

          # if the current Project entity has any assets, extract them
          if (!is.null(has_part)) {
            # extract assets by @id
            assets_tbl <- y |>
              flatten_safe_data(id = has_part)

            proj_assets_tbl <- tibble::tibble(
              project_id = project_id,
              project = project
            ) |>
              dplyr::bind_cols(assets_tbl)

            return(proj_assets_tbl)
          }
          return(tibble::tibble(
            project_id = project_id,
            project = project,
            asset_id = NA,
            asset = NA
          ))
        }) |>
        dplyr::bind_rows()
    },
    error = function(e) {
      tibble::tibble()
    }
  )
}

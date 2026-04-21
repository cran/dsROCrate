#' Extract Safe Data entity(ies)
#'
#' @inheritParams safe_data
#' @param ... Other optional arguments. See the full documentation
#'
#' @returns RO-Crate with Safe Data entity(ies).
#' @rdname extract_safe_data
#' @keywords internal
extract_safe_data <- function(x, ...) {
  UseMethod("extract_safe_data")
}

#' @param rocrate (Optional) RO-Crate object to update with Safe Data details.
#' @rdname extract_safe_data
#' @export
extract_safe_data.opal <- function(x, ..., rocrate = rocrateR::rocrate_5s()) {
  # extract all data sources
  ds <- opalr::opal.datasources(x)

  # extract project names and ignore NAs
  projects <- ds$name[!is.na(ds$name) & !is.null(ds$name)]

  # create RO-Crate with Safe Data details for the projects found on the server
  purrr::reduce(
    projects,
    \(crate, p) {
      safe_data(crate, connection = x, project = p)
    },
    .init = rocrate
  )
}

#' @param id (Optional) Vector with `@id` strings for Safe Data entity(ies)
#'     to be extracted from the given RO-Crate, `x`.
#' @rdname extract_safe_data
#' @export
extract_safe_data.rocrate <- function(
  x,
  ...,
  id = NULL,
  asset_id_suffix = "#asset:",
  rocrate = rocrateR::rocrate_5s()
) {
  # validate RO-Crate
  rocrateR::is_rocrate(x)

  # filter out asset entities associated with the project based on the
  # value for `asset_id_suffix`.
  entities_lst <- x$`@graph` |>
    purrr::keep(\(x) grepl(paste0("^", asset_id_suffix), x$`@id`))

  # if `id` was provided, then filter out only those entities
  if (!is.null(id)) {
    entities_lst <- entities_lst |>
      purrr::keep(\(x) getElement(x, "@id") %in% id)
  }

  # check if any entities were found
  if (length(entities_lst) == 0) {
    stop("No matching entities were found!", call. = FALSE)
  } else {
    message(
      length(entities_lst),
      " asset entit",
      ifelse(length(entities_lst) == 1, "y was", "ies were"),
      " found!"
    )
  }

  # add entities to the RO-Crate
  suppressWarnings({
    purrr::reduce(entities_lst, rocrateR::add_entity, .init = rocrate)
  })
}

#' Flatten object with Safe Data details
#'
#' @param x Object (e.g., RO-Crate) with Safe Data details. This can be
#'     generated with the [extract_safe_data()] function.
#' @param id Vector of strings with the `@id`s for the datasets to be extracted.
#'     If not provided, extract all entities with `@type = 'Dataset'`.
#'
#' @returns Data frame with Safe Data details.
#' @rdname flatten_safe_data
#' @keywords internal
flatten_safe_data <- function(x, ...) {
  UseMethod("flatten_safe_data")
}

#' @rdname flatten_safe_data
#' @export
flatten_safe_data.default <- function(x, ...) {
  return(tibble::tibble())
}

#' @rdname flatten_safe_data
#' @export
flatten_safe_data.rocrate <- function(
  x,
  ...,
  id = NULL,
  asset_id_suffix = "#asset:"
) {
  # local bindings
  asset_id <- person_id <- NULL

  tryCatch(
    {
      # extract asset entities
      entities_tbl <- x$`@graph` |>
        purrr::keep(\(x) grepl(paste0("^", asset_id_suffix), x$`@id`)) |>
        # extract @id and name for each entity
        lapply(function(ent) {
          tibble::tibble(
            asset_id = getElement(ent, "@id"),
            asset = getElement(ent, "name")
          )
        }) |>
        # combine all rows
        dplyr::bind_rows() |>
        # filter out root (./) entity
        dplyr::filter(asset_id != "./")

      # if `id` is provided, then only keep those entities
      if (!is.null(id)) {
        entities_tbl <- entities_tbl |>
          dplyr::filter(asset_id %in% !!id)
      }

      # return dataset entities
      return(entities_tbl)
    },
    error = function(e) {
      tibble::tibble()
    }
  )
}

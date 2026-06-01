#' Extract Safe Setting entity(ies)
#'
#' @inheritParams safe_data
#' @param ... Other optional arguments. See the full documentation
#' @param rocrate (Optional) RO-Crate object to update with Safe Setting details.
#' @param id (Optional) Vector with `@id` strings for Safe Setting entity(ies).
#'
#' @returns RO-Crate with Safe Setting entity(ies).
#' @keywords internal
#' @noRd
extract_safe_setting <- function(x, ...) {
  UseMethod("extract_safe_setting")
}

#' @export
extract_safe_setting.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s()
) {
  # extract all the Safe Settings for the current Opal connection
  rocrate <- safe_setting(x, rocrate = rocrate)

  # return RO-Crate with Safe Setting details
  return(rocrate)
}

#' @export
extract_safe_setting.rocrate <- function(
  x,
  ...,
  id = NULL,
  rocrate = rocrateR::rocrate_5s()
) {
  # validate RO-Crate
  rocrateR::is_rocrate(x)

  # extract Safe Setting root entity, type = 'CreativeWork'
  ss_root <- .get_entity(x, type = "CreativeWork") |>
    purrr::keep(\(x) grepl("^#safesetting", x[["@id"]]))

  if (is.null(ss_root)) {
    stop("No Safe Setting root entity was found!", call. = FALSE)
  }

  # extract entities linked to the root
  part_ids <- ss_root |>
    purrr::map("hasPart") |>
    purrr::list_c() |>
    purrr::map(`[[`, "@id") |>
    # purrr::map(~ purrr::map(.x, "@id")) |>
    purrr::list_c()
  # ss_root_ids <- ss_root[[1]]$hasPart |>
  #   purrr::map(`[[`, "@id") |>
  #   purrr::list_c()

  # extract sub-entities linked to entities directly linked to the root entity
  if (length(part_ids) == 0) {
    stop("The Safe Setting root entity has no entities linked!", call. = FALSE)
  }

  entities_lst <- .get_entity(x, id = part_ids)
  sub_part_ids <- entities_lst |>
    purrr::map("hasPart") |>
    purrr::list_c() |>
    purrr::map(`[[`, "@id") |>
    purrr::list_c()
  if (length(sub_part_ids) > 0) {
    entities_lst <- c(entities_lst, .get_entity(x, id = sub_part_ids))
  }

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
      " 'CreativeWork', 'PropertyValue' OR 'SoftwareApplication' entit",
      ifelse(length(entities_lst) == 1, "y was", "ies were"),
      " found!"
    )
  }

  # add entities to the RO-Crate and
  # ignore warnings about existing permission entities
  # and return RO-Crate with the Safe Setting details
  suppressWarnings({
    purrr::reduce(
      entities_lst,
      rocrateR::add_entity,
      overwrite = TRUE,
      .init = rocrate
    )
  })
}

#' Flatten object with Safe Setting details
#'
#' @param x Object (e.g., RO-Crate) with Safe Setting details. This can be
#'     generated with the `extract_safe_setting()` function.
#' @param id Vector of strings with the `@id`s for the settings to be extracted.
#'     If not provided, extract all entities with `@type = 'PropertyValue'` or
#'     `@type = 'SoftwareApplication'`.
#'
#' @returns Data frame with Safe Settings.
#' @keywords internal
#' @noRd
flatten_safe_setting <- function(x, ...) {
  UseMethod("flatten_safe_setting")
}

#' @export
flatten_safe_setting.default <- function(x, ...) {
  return(tibble::tibble())
}

#' @export
flatten_safe_setting.rocrate <- function(x, ..., id = NULL) {
  tryCatch(
    {
      # extract entities
      entities_tbl <- x |>
        .get_entity(type = c("PropertyValue", "SoftwareApplication")) |>
        # extract @id, name, value and version for each entity
        lapply(function(ent) {
          tibble::tibble(
            id = getElement(ent, "@id"),
            name = getElement(ent, "name"),
            value = getElement(ent, "value"),
            version = getElement(ent, "version")
          )
        }) |>
        # combine all rows
        dplyr::bind_rows()

      # if `id` is provided, then only keep those entities
      if (!is.null(id)) {
        entities_tbl <- entities_tbl |>
          dplyr::filter(id %in% !!id)
      }

      # return dataset entities
      return(entities_tbl)
    },
    error = function(e) {
      tibble::tibble()
    }
  )
}

#' Link Safe Setting entity to all Safe Projects
#'
#' Creates explicit graph relationships so Safe Settings are
#' discoverable from Project-level governance reports.
#'
#' Relationship model:
#'   Safe Setting <root> --> isPartOf --> Project
#'
#' @param rocrate RO-Crate object, see [rocrateR::rocrate].
#' @return Updated rocrate.
#' @keywords internal
#' @noRd
link_safe_settings_to_projects <- function(rocrate) {
  ents <- rocrate$`@graph`

  setting_ids <- purrr::map(
    purrr::keep(ents, ~ grepl("^#safesetting", .x$`@id` %||% "")),
    "@id"
  ) |>
    purrr::list_c()

  project_ids <- purrr::map(
    purrr::keep(ents, ~ identical(.x$`@type`, "Project")),
    "@id"
  ) |>
    purrr::list_c()

  if (length(setting_ids) == 0 || length(project_ids) == 0) {
    return(rocrate)
  }

  link_entities <- expand.grid(setting = setting_ids, proj = project_ids) |>
    purrr::pmap(
      function(setting, proj) {
        rocrateR::entity(
          id = id_hash("#link:", paste0(setting, proj)),
          type = "CreativeWork",
          name = "Safe Settings x Safe Project Link",
          about = list("@id" = setting),
          isPartOf = list("@id" = proj)
        )
      }
    )

  purrr::reduce(link_entities, rocrateR::add_entity, .init = rocrate)
}

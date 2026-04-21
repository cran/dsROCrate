#' Append entity reference
#'
#' Adds a reference to an existing property while
#' preserving existing values and avoiding duplicates.
#'
#' @param rocrate RO-Crate object.
#' @param id String with `@id` of the entity.
#' @param key String with property/key of the entity.
#' @param ref_id String with reference `@id`.
#'
#' @returns Updated RO-Crate object.
#' @keywords internal
append_entity_ref <- function(rocrate, id, key, ref_id) {
  entity <- rocrateR::get_entity(rocrate, id = id)[[1]]

  current <- entity[[key]]

  existing_ids <- if (!is.null(current)) {
    getElement(current, "@id") %||% purrr::map_chr(current, "@id")
  } else {
    character()
  }

  ids <- unique(c(existing_ids, ref_id))

  value <- purrr::map(ids, ~ list("@id" = .x))

  rocrateR::add_entity_value(
    rocrate,
    id = id,
    key = key,
    value = value,
    overwrite = TRUE
  )
}

#' Wrapper for [rocrateR::get_entity()]
#'
#' This wrapper is used to suppress warning messages like
#' 'No matching entities were found with ...'.
#'
#' @importFrom rocrateR get_entity
#'
#' @returns List with entity of objects
#' @noRd
.get_entity <- function(rocrate, id = NULL, type = NULL) {
  suppressWarnings(rocrateR::get_entity(rocrate, id = id, type = type))
}

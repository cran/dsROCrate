#' Extract Safe People entity(ies)
#'
#' @inheritParams safe_data
#' @param ... Other optional arguments. See the full documentation
#'
#' @returns RO-Crate with Safe People entity(ies).
#' @rdname extract_safe_people
#' @keywords internal
extract_safe_people <- function(x, ...) {
  UseMethod("extract_safe_people")
}

#' @param rocrate (Optional) RO-Crate object to update with Safe People details.
#' @rdname extract_safe_people
#' @export
extract_safe_people.opal <- function(x, ..., rocrate = rocrateR::rocrate_5s()) {
  # set local binding
  name <- principal <- NULL

  # extract all users
  opal_users <- opalr::oadmin.user_profiles(x, df = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::rename(name = principal) |>
    # exclude system administrators from the report
    dplyr::filter(!(tolower(name) %in% c("admin", "administrator")))

  # cycle through the data source (x) and extract project details
  for (i in seq_len(nrow(opal_users))) {
    username <- opal_users[i, "name"]
    if (!is.na(username) && !is.null(username)) {
      suppressWarnings({
        rocrate <- rocrate |>
          safe_people(user = username, connection = x, set_author = FALSE)
      })
    }
  }

  # return RO-Crate with Safe Project details
  return(rocrate)
}

#' @param id (Optional) Vector with `@id` strings for Safe People entity(ies)
#'     to be extracted from the given RO-Crate, `x`.
#' @rdname extract_safe_people
#' @export
extract_safe_people.rocrate <- function(
  x,
  ...,
  id = NULL,
  rocrate = rocrateR::rocrate_5s()
) {
  # if `id` wasn't provided, then extract from root (./) entity of the RO-Crate
  if (is.null(id)) {
    # extract author `@id`s from the root directory
    id <- .get_entity(x, id = "./", type = "Dataset") |>
      lapply(\(x) getElement(x, "author")) |>
      sapply(\(x) getElement(x, "@id")) |>
      unlist()
  }

  # extract entities with type = 'Person'
  entities_lst <- .get_entity(x, type = "Person")

  # filter out person-entities in the `id`, if `id` is not NULL
  if (length(id) && !is.null(id)) {
    idx <- entities_lst |>
      sapply(\(x) getElement(x, "@id") %in% id)
    entities_lst_v2 <- entities_lst[idx]
  } else {
    entities_lst_v2 <- entities_lst
  }

  # check if any entities were found
  if (length(entities_lst_v2) == 0) {
    stop(
      "No matching entities were found for the Author(s) in the root ",
      "entity (./):\n",
      paste0("  - ", id, collapse = "\n"),
      call. = FALSE
    )
  } else {
    message(
      length(entities_lst_v2),
      " 'Author' entit",
      ifelse(length(entities_lst_v2) == 1, "y was", "ies were"),
      " found!"
    )
  }

  # add entities to the RO-Crate
  rocrate <- rocrate |>
    rocrateR::add_entity(entities_lst_v2, overwrite = TRUE, verbose = FALSE)
  # # link new user entity @id to the root (./) author property
  # rocrateR::add_entity_value(
  #   id = "./",
  #   key = "author",
  #   value = list(`@id` = getElement(entities_lst_v2, "@id"))
  # )

  # return RO-Crate with the Safe People details
  return(rocrate)
}

#' Flatten object with Safe People details
#'
#' @param x Object (e.g., RO-Crate) with Safe People details. This can be
#'     generated with the [extract_safe_data()] function.
#' @param ... Other optional arguments (not in used).
#' @param id Vector of strings with the `@id`s for the users to be extracted.
#'     If not provided, extract all entities with `@type = 'Person'`.
#'
#' @returns Data frame with safe people details.
#' @rdname flatten_safe_people
#' @keywords internal
flatten_safe_people <- function(x, ...) {
  UseMethod("flatten_safe_people")
}

#' @rdname flatten_safe_people
#' @export
flatten_safe_people.default <- function(x, ...) {
  return(tibble::tibble())
}

#' @rdname flatten_safe_people
#' @export
flatten_safe_people.rocrate <- function(x, ..., id = NULL) {
  # local bindings
  person_id <- NULL
  tryCatch(
    {
      # extract Person entities
      entities_tbl <- x |>
        .get_entity(type = "Person") |>
        # extract @id and name for each entity
        lapply(function(ent) {
          tibble::tibble(
            person_id = getElement(ent, "@id"),
            name = getElement(ent, "name"),
            given_name = c(
              getElement(ent, "givenName"),
              getElement(ent, "given_name")
            ),
            family_name = c(
              getElement(ent, "familyName"),
              getElement(ent, "family_name")
            ),
            organisation = getElement(ent, "organisation")
          )
        }) |>
        # combine all rows
        dplyr::bind_rows()

      # if `id` is provided, then only keep those entities
      if (!is.null(id)) {
        entities_tbl <- entities_tbl |>
          dplyr::filter(person_id %in% !!id)
      }

      # return dataset entities
      return(entities_tbl)
    },
    error = function(e) {
      tibble::tibble()
    }
  )
}

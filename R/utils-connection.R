#' Parse user profiles
#'
#' @inheritParams project_exists
#' @param ... Optional arguments, unused.
#'
#' @returns Data frame with given `user`'s profile details, as captured on the
#' server pointed by `x`.
#' @keywords internal
#' @aliases parse_user_profiles,armadillo-method
#' @family Armadillo
#' @usage
#' \S4method{parse_user_profiles}{armadillo}(x, ..., user)
parse_user_profiles <- function(x, ...) {
  UseMethod("parse_user_profiles")
}

# S3 methods ----
#' @rdname parse_user_profiles
#' @family Opal
parse_user_profiles.opal <- function(x, ..., user) {
  # local bindings
  principal <- userInfo <- NULL

  # get user profiles and filter by the current user
  user_prof_tbl <- opalr::oadmin.user_profiles(x, df = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::filter(principal %in% user)
  # extract (if available) `userInfo` which contains additional details
  if (nrow(user_prof_tbl) > 0 && "userInfo" %in% colnames(user_prof_tbl)) {
    user_prof_tbl <- user_prof_tbl |>
      dplyr::mutate(
        userInfo = userInfo |>
          lapply(function(x) {
            if (is.na(x)) {
              tibble::tibble()
            } else {
              x |>
                jsonlite::fromJSON() |>
                tibble::as_tibble()
            }
          })
      )
  }
  return(user_prof_tbl)
}

# S4 methods ----
#' @method parse_user_profiles ArmadilloCredentials
#' @rdname parse_user_profiles
#' @export
parse_user_profiles.ArmadilloCredentials <- function(x, ..., user) {
  message("PLACEHOLDER!")
}

#' Verify if project exists
#'
#' Wrapper for the [opalr::opal.project_exists()] and
#' [MolgenisArmadillo::armadillo.list_projects()] functions.
#'
#' @param x Connection object to backend for DataSHIELD server (e.g., Opal).
#' @param ... Optional arguments, unused.
#' @param project String with project name to be verified.
#'
#' @returns Nothing, call for its side effect. Stop execution of script if
#' `project` does not exist in the given server.
#'
#' @keywords internal
#' @aliases project_exists,armadillo-method
#' @family Armadillo
#' @usage
#' \S4method{project_exists}{armadillo}(
#'   x,
#'   ...,
#'   project
#' )
project_exists <- function(x, ...) {
  UseMethod("project_exists")
}

# S3 methods ----
#' @rdname project_exists
#' @family Opal
project_exists.opal <- function(x, ..., project) {
  if (!opalr::opal.project_exists(x, project)) {
    stop(
      sprintf(
        "The `project = '%s'` was not found in the given Opal connection!",
        project
      ),
      call. = FALSE
    )
  }
}

# S4 methods ----
#' @method project_exists ArmadilloCredentials
#' @rdname project_exists
#' @export
#' @family Armadillo
project_exists.ArmadilloCredentials <-
  function(
    x,
    ...,
    project
  ) {
    if (!(project %in% MolgenisArmadillo::armadillo.list_projects())) {
      stop(
        sprintf(
          "The `project = '%s'` was not found in the given Armadillo connection!",
          project
        ),
        call. = FALSE
      )
    }
  }

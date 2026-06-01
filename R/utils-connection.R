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
#' @noRd
parse_user_profiles <- function(x, ...) {
  UseMethod("parse_user_profiles")
}

# S3 methods ----
#' @export
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
#' @noRd
project_exists <- function(x, ...) {
  UseMethod("project_exists")
}

# S3 methods ----
#' @export
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

#' Validate backend
#'
#' Validate backend: including connection status, backend version and check the
#' user permissions.
#'
#' @param x DataSHIELD backend connection object.
#' @param ... Optional params.
#'
#' @returns Nothing, call for its side effect.
#' @keywords internal
#' @noRd
validate_backend <- function(x, ...) {
  # validate connection object
  validate_con(x, ...)
  # validate backend version
  validate_backend_version(x, ...)
  # validate that the connection user has administrative or audit privileges
  check_permissions(x, ...)

  invisible(TRUE)
}

#' Validate backend version
#'
#' @param x DataSHIELD backend connection object.
#' @param ... Unused.
#' @param minimum String with minimum version.
#'
#' @returns Logical value indicating if backend version is valid.
#' @keywords internal
#' @noRd
validate_backend_version <- function(x, ...) {
  UseMethod("validate_backend_version")
}

#' @export
validate_backend_version.default <- function(x, ...) {
  invisible(TRUE)
}

#' @export
validate_backend_version.opal <- function(x, ..., minimum = "5.7.2") {
  if (utils::compareVersion(x$version, minimum) < 0) {
    stop(
      sprintf(
        "Opal >= %s is required, but server version is %s.",
        minimum,
        x$version
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate backend connection
#'
#' @param x DataSHIELD backend connection object.
#' @param ... Unused.
#'
#' @returns Nothing, call for its side effect.
#' @keywords internal
#' @noRd
validate_con <- function(x, ...) {
  UseMethod("validate_con")
}

#' @export
validate_con.default <- function(x, ...) {
  stop(
    sprintf(
      "Unsupported connection type: %s",
      paste(class(x), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @export
validate_con.opal <- function(x, ...) {
  tryCatch(
    {
      status <- xptr::is_null_xptr(x$handle$handle)
      if (status) {
        stop("The given connection is not valid!", call. = FALSE)
      }
    },
    error = function(e) {
      stop("The given connection is not valid!", call. = FALSE)
    }
  )
}

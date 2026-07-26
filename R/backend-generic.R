backend_logs <- function(x, ...) {
  UseMethod("backend_logs")
}

backend_options <- function(x, ...) {
  UseMethod("backend_options")
}

backend_packages <- function(x, ...) {
  UseMethod("backend_packages")
}

backend_profile_exists <- function(x, ...) {
  UseMethod("backend_profile_exists")
}

backend_project <- function(x, ...) {
  UseMethod("backend_project")
}

backend_project_exists <- function(x, ...) {
  UseMethod("backend_project_exists")
}

backend_project_perms <- function(x, ...) {
  UseMethod("backend_project_perms")
}

backend_projects <- function(x, ...) {
  UseMethod("backend_projects")
}

backend_resource_perms <- function(x, ...) {
  UseMethod("backend_resource_perms")
}

backend_resources <- function(x, ...) {
  UseMethod("backend_resources")
}

backend_sys_perms <- function(x, ...) {
  UseMethod("backend_sys_perms")
}

backend_table_perms <- function(x, ...) {
  UseMethod("backend_table_perms")
}

backend_tables <- function(x, ...) {
  UseMethod("backend_tables")
}

backend_user_exists <- function(x, ...) {
  UseMethod("backend_user_exists")
}

backend_users <- function(x, ...) {
  UseMethod("backend_users")
}

#' Verify if connection was created by an administrator user
#'
#' @inheritParams validate_con
#' @param ... Unused, extra arguments.
#'
#' @returns Boolean flag to indicate whether the given connection was created
#'     by an administrator user.
#' @keywords internal
#'
#' @noRd
is_admin_con <- function(x, ...) {
  UseMethod("is_admin_con")
}

#' Verify if connection was created by an auditor user
#'
#' @inheritParams validate_con
#' @param ... Unused, extra arguments.
#'
#' @returns Boolean flag to indicate whether the given connection was created
#'     by an auditor user.
#' @keywords internal
#'
#' @noRd
is_audit_con <- function(x, ...) {
  UseMethod("is_audit_con")
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

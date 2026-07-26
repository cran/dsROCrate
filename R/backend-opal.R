#' @export
backend_logs.opal <- function(x, ...) {
  opalr::dsadmin.log(x, ...)
}

#' @export
backend_options.opal <- function(x, ...) {
  opalr::dsadmin.get_options(x, ...)
}

#' @export
backend_packages.opal <- function(x, ...) {
  opalr::dsadmin.package_descriptions(x, ...)
}

#' @export
backend_profile_exists.opal <- function(x, ...) {
  opalr::dsadmin.profile_exists(x, ...)
}

#' @export
backend_project.opal <- function(x, ...) {
  opalr::opal.project(x, ...)
}

#' @export
backend_project_exists.opal <- function(x, ...) {
  opalr::opal.project_exists(x, ...)
}

#' @export
backend_project_perms.opal <- function(x, ...) {
  opalr::opal.project_perm(x, ...)
}

#' @export
backend_projects.opal <- function(x, ...) {
  opalr::opal.projects(x, ...)
}

#' @export
backend_resource_perms.opal <- function(x, ...) {
  opalr::opal.resource_perm(x, ...)
}

#' @export
backend_resources.opal <- function(x, ...) {
  opalr::opal.resources(x, ...)
}

#' @export
backend_sys_perms.opal <- function(x, ...) {
  opalr::oadmin.system_perm(x, ...)
}

#' @export
backend_table_perms.opal <- function(x, ...) {
  opalr::opal.table_perm(x, ...)
}

#' @export
backend_tables.opal <- function(x, ...) {
  opalr::opal.tables(x, ...)
}

#' @export
backend_user_exists.opal <- function(x, ...) {
  opalr::oadmin.user_exists(x, ...)
}

#' @export
backend_users.opal <- function(x, ...) {
  opalr::oadmin.user_profiles(x, ...)
}

#' @export
is_admin_con.opal <- function(x, ...) {
  # condition 1: admin users have access to `backend_user_exists`
  cond1 <- .try_load(backend_user_exists(x, x$username))
  # condition 2: admin users have access to `backend_profile_exists`
  cond2 <- .try_load(backend_profile_exists(x, "default"))

  # check all the conditions are met
  result <- all(!is.null(cond1$value), !is.null(cond2$value))
  attr(result, "error") <- list(
    user_exists = cond1$error,
    profile_exists = cond2$error
  )
  result
}

#' @export
is_audit_con.opal <- function(x, ...) {
  # condition 1: admin users have access to `backend_user_exists`
  cond1 <- .try_load(backend_user_exists(x, x$username))
  # condition 2: admin users have access to `backend_profile_exists`
  cond2 <- .try_load(backend_profile_exists(x, "default"))

  # check all the conditions are met
  # cond1 is met when the call *errored* (no admin access, as expected of
  # a pure auditor); cond2 is met when the call *succeeded* - the returned
  # values don't matter either way
  result <- all(!is.null(cond1$error), !is.null(cond2$value))
  attr(result, "error") <- list(
    user_exists = NULL, # an error here is expected/correct for an auditor,
    profile_exists = cond2$error
  )
  result
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

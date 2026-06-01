#' Check backend connection permissions
#'
#' Validates whether a backend connection has sufficient permissions for
#' `{dsROCrate}` audit operations.
#'
#' Currently, audit or administrator permissions are required.
#'
#' @param x A backend connection object.
#' @param ... Additional arguments passed to methods.
#'
#' @returns
#' Returns `TRUE` invisibly if the connection has sufficient permissions.
#'
#' @export
#'
#' @seealso
#' \code{vignette("audit-permissions", package = "dsROCrate")}
check_permissions <- function(x, ...) {
  UseMethod("check_permissions")
}

#' @export
check_permissions.default <- function(x, ...) {
  stop(
    sprintf(
      paste0(
        "No `check_permissions()` method exists for objects of class: %s.\n",
        "Please provide a supported backend connection object."
      ),
      paste(class(x), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @export
check_permissions.opal <- function(x, ...) {
  is_admin <- FALSE
  is_audit <- FALSE

  is_admin <- tryCatch(
    is_opal_admin_con(x),
    error = function(e) FALSE
  )

  is_audit <- tryCatch(
    is_opal_audit_con(x),
    error = function(e) FALSE
  )

  if (isTRUE(is_admin) || isTRUE(is_audit)) {
    return(invisible(TRUE))
  }

  stop(
    paste(
      "The supplied backend connection does not have sufficient permissions.",
      "",
      "{dsROCrate} requires elevated permissions to perform audit operations.",
      "",
      "Please see:",
      "  vignette('audit-permissions', package = 'dsROCrate')",
      "",
      "for backend-specific configuration instructions.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

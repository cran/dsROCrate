#' Check backend connection permissions
#'
#' Validates whether a backend connection has sufficient permissions for
#' `{dsROCrate}` audit operations.
#'
#' Currently, audit or administrator permissions are required.
#'
#' @param x A backend connection object.
#' @param ... Additional arguments passed to methods.
#' @param verbose Boolean value used to indicate if a success message should be
#'     displayed (default: FALSE)
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

# @rdname check_permissions
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

#' @rdname check_permissions
#' @export
check_permissions.ArmadilloCredentials <- function(x, ...) {
  stop(
    paste0(
      "`check_permissions()` for the Armadillo backend is ",
      "not currently implemented!"
    ),
    call. = FALSE
  )
}

#' @rdname check_permissions
#' @export
check_permissions.opal <- function(x, ..., verbose = FALSE) {
  is_admin <- FALSE
  is_audit <- FALSE

  is_admin <- tryCatch(
    is_admin_con(x),
    error = function(e) FALSE
  )

  is_audit <- tryCatch(
    is_audit_con(x),
    error = function(e) FALSE
  )

  if (isTRUE(is_admin) || isTRUE(is_audit)) {
    if (verbose) {
      message("You are ready to audit this system!")
    }
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

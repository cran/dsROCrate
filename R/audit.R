#' Create an audit RO-Crate
#'
#' Create an audit RO-Crate following the 5 Safes Principles.
#'
#' This function handles various audit types, which will be dispatched based on
#' the input object. If the input object is
#'
#' \itemize{
#'     \item a _connection_ to a DataSHIELD server (e.g., OBiBa's Opal):
#'.    generates an RO-Crate object with deployment details, including outputs.
#'     \item a _path_ pointing to
#'     \itemize{
#'         \item **a `cr8tor` archive / governance bundle**: generates an
#'         RO-Crate object with pre-deployment governance details.
#'         \item **an RO-Crate object**: generates an RO-Crate object with
#'         clearly defined 5 Safes elements.
#'     }
#'     \item an _RO-Crate_ object: generates an RO-Crate object with clearly
#'     defined 5 Safes elements.
#' }
#' @param x Object to be audited. This can be
#'     \itemize{
#'         \item a _connection_ to a DataSHIELD server (e.g., OBiBa's Opal).
#'         \item a _path_ pointing to an RO-Crate OR a `cr8tor` archive /
#'.        governance bundle.
#'         \item an _RO-Crate_ object.
#'     }
#'     Alternatively, a list of any of the above.
#' @param ... Additional arguments.
#' @param intent Additional object with governance bundle/specification of the
#'     intent of a project. It takes the same types as `x`.
#' @param profile String with profile name (used for OBiBa's Opal backend).
#' @param project String with project name(s) from which to extract Safe Project
#'     details.
#' @param resources Vector of strings with the names of the resources, part of
#'     `project`. Optional, if not provided, all the resources associated to
#'     `project` will be included in the RO-Crate.
#' @param tables Vector of strings with the names of the tables/datasets, part
#'     of `project`. Optional, if not provided, all the tables/datasets
#'     associated to `project` will be included in the RO-Crate.
#' @param user String with the user name for which to extract Safe People
#'     details.
#' @param logs_from Lower limit timestamp to filter out the outputs generated
#'     (default: `-Inf`, everything up to `logs_to`)
#' @param logs_to Upper limit timestamp to filter out the outputs generated
#'     (default: `Inf`, everything from `logs_from` onwards).
#' @param path String with path pointing to the root of the RO-Crate. This will
#'     be used to store log files. If not provided, logs will be stored within
#'     the RO-Crate returned by this function.
#'
#' @returns RO-Crate with audit details.
#' @export
audit <- function(x, ...) {
  UseMethod("audit")
}

#' @export
audit.default <- function(x, ...) {
  stop(
    sprintf(
      "No `audit()` method exists for objects of class: %s.",
      paste(class(x), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @rdname audit
#' @export
audit.ArmadilloCredentials <- function(x, ..., intent = NULL) {
  stop(
    "The `audit()` for the Armadillo backend is not currently implemented!",
    call. = FALSE
  )
}

#' @rdname audit
#' @export
audit.character <- function(x, ..., intent = NULL) {
  # verify if the given path exists, if not, return an error message
  if (!file.exists(x)) {
    stop("The given file does not exist!", call. = FALSE)
  }

  # attempt loading a `cr8tor` bundle
  cr8tor_res <- .try_load(load_cr8tor_bundle(x, ...))
  x_obj <- cr8tor_res$value

  # alternatively, attempt loading an RO-Crate
  rocrate_res <- list(value = NULL, error = NULL)
  if (is.null(x_obj)) {
    rocrate_res <- .try_load(rocrateR::load_rocrate(x, ...))
    x_obj <- rocrate_res$value
  }

  if (is.null(x_obj)) {
    stop(
      paste0(
        "The given path does not point to a valid `cr8tor` archive nor an ",
        "`rocrate`.\n\n",
        "  cr8tor bundle error:  ",
        if (is.null(cr8tor_res$error)) "(not attempted)" else cr8tor_res$error,
        "\n",
        "  rocrate error:        ",
        if (is.null(rocrate_res$error)) "(not attempted)" else rocrate_res$error
      ),
      call. = FALSE
    )
  }

  # attempt auditing intent
  intent_lst <- audit_intent(intent, ...)

  # call next method
  main_audit <- audit(x_obj, intent_lst$main_audit_args)

  # return list
  if (is.null(intent_lst$intent_audit)) {
    return(main_audit)
  }
  list(intent = intent_lst$intent_audit, deployment = main_audit)
}

#' @rdname audit
#' @export
audit.cr8tor <- function(x, ..., intent = NULL) {
  # attempt auditing intent
  intent_lst <- audit_intent(intent, ..., excluded_args = "path")

  # call next method
  main_audit <- audit_engine(x, intent_lst$main_audit_args)

  # return list
  if (is.null(intent_lst$intent_audit)) {
    return(main_audit)
  }
  list(intent = intent_lst$intent_audit, deployment = main_audit)
}

#' @rdname audit
#' @export
audit.list <- function(x, ..., intent = NULL) {
  purrr::map(x, audit, ..., intent = intent)
}

#' @rdname audit
#' @export
audit.opal <- function(
  x,
  ...,
  intent = NULL,
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL,
  user = NULL,
  logs_from = -Inf,
  logs_to = Inf,
  path = NULL
) {
  # attempt auditing intent
  intent_lst <- audit_intent(intent, ...)

  # call next method
  main_audit <- audit_engine(
    x,
    profile = profile,
    project = c(intent_lst$main_audit_args$project, project),
    resources = resources,
    tables = tables,
    user = c(intent_lst$main_audit_args$user, user),
    logs_from = logs_from,
    logs_to = logs_to,
    path = path,
    intent_lst$main_audit_args
  )

  # return list
  if (is.null(intent_lst$intent_audit)) {
    return(main_audit)
  }
  list(intent = intent_lst$intent_audit, deployment = main_audit)
}

#' @rdname audit
#' @export
audit.rocrate <- function(x, ..., intent = NULL) {
  # return input rocrate object
  x
}

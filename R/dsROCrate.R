#' Initialise a Five Safes RO-Crate
#'
#' Creates a new RO-Crate configured for Five Safes auditing.
#'
#' @param x This can be a connection to a 'DataSHIELD' server (e.g., object with
#'     the `opal` or `ArmadilloCredentials` classes), an RO-Crate
#'     ([rocrate][rocrateR::rocrate()] class) or a string with the path to an
#'     RO-Crate.
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::init`][init()].
#' @param connection Connection object for the 'DataSHIELD' server where the
#'     values will be extracted from (e.g., OBiBa's Opal). Optional, if `x` is
#'     set to a connection object. If so, then `rocrate` is required.
#' @param rocrate RO-Crate object. Optional, if `x` is either an RO-Crate
#'     object or a path to a valid RO-Crate. If so, then `connection` is
#'     required (default: `rocrateR::rocrate_5s()`).
#' @param path String with path pointing to the root of the RO-Crate. This will
#'     be used to store log files. If not provided, logs will be stored within
#'     the RO-Crate returned by this function.
#' @param profile String with profile name (used for OBiBa's Opal backend).
#' @param project String with the name of the [Safe Project][safe_project()].
#' @param resources Vector of strings with the names of the resources, part of
#'     `project`. Optional, if not provided, all the resources associated to
#'     `project` will be included in the RO-Crate.
#' @param tables Vector of strings with the names of the tables/datasets, part
#'     of `project`. Optional, if not provided, all the tables/datasets
#'     associated to `project` will be included in the RO-Crate.
#' @param user List (or [entity][rocrateR::entity()] object) with details for
#'     the Safe People, it must include `@id` and `name` entries. Alternatively,
#'     this can be a string with the `name` of the current user.
#'
#' @returns Five Safes RO-Crate object.
#'
#' @references
#' Wilkinson, M., Dumontier, M., Aalbersberg, I. et al. (2016) The FAIR Guiding
#' Principles for scientific data management and stewardship. Sci Data 3,
#' 160018. https://doi.org/10.1038/sdata.2016.18
#'
#' @export
init <- function(x, ...) {
  UseMethod("init")
}

#' @rdname init
#' @export
init.ArmadilloCredentials <- function(x, ...) {
  stop(
    "`init()` for the Armadillo backend is not currently implemented!",
    call. = FALSE
  )
}

#' @rdname init
#' @export
init.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s(),
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL,
  path = NULL,
  user = NULL
) {
  # validate backend
  validate_backend(x, ...)

  # attach input arguments as attributes
  attr(rocrate, "connection") <- x
  attr(rocrate, "path") <- path
  attr(rocrate, "profile") <- profile
  attr(rocrate, "project") <- project
  attr(rocrate, "resources") <- resources
  attr(rocrate, "tables") <- tables
  attr(rocrate, "user") <- user

  return(rocrate)
}

#' @rdname init
#' @export
init.rocrate <- function(
  x,
  ...,
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables"),
  user = attr(x, "user")
) {
  init(
    connection,
    rocrate = x,
    path = path,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables,
    user = user
  )
}

# helper functions ----
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b

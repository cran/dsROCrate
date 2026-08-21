#' Audit Engine
#'
#' Internal function to create audits for various back-ends.
#'
#' @param x This can be a connection to a 'DataSHIELD' server (e.g., object with
#'     the `opal` or `ArmadilloCredentials` classes). Alternatively, a
#'     governance archive file, representing the intent of a project and
#'     associated governance details.
#' @param ... Other optional arguments, see full documentation for details.
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
#' @returns Audit RO-Crate with 5 Safes Components.
#' @keywords internal
#' @noRd
audit_engine <- function(x, ...) {
  UseMethod("audit_engine")
}

#' @export
audit_engine.default <- function(x, ...) {
  stop(
    sprintf(
      "No `audit_engine()` method exists for objects of class: %s.",
      paste(class(x), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' @export
audit_engine.cr8tor <- function(x, ...) {
  # extract individual components from cr8tor bundle
  audit <- list(
    metadata = extract_cr8tor_metadata(x),
    integrity = extract_integrity_cr8tor(x),
    safe_people = extract_safe_people_cr8tor(x),
    safe_projects = extract_safe_projects_cr8tor(x),
    safe_data = extract_safe_data_cr8tor(x),
    safe_settings = extract_safe_settings_cr8tor(x),
    safe_outputs = extract_safe_outputs_cr8tor(x),
    user_projects = extract_user_projects_cr8tor(x),
    user_groups = extract_user_groups_cr8tor(x),
    groups = extract_groups_cr8tor(x),
    permissions = extract_permissions_cr8tor(x)
  )

  as_rocrate_audit(audit)
}

#' @export
audit_engine.opal <- function(
  x,
  ...,
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL,
  user = NULL,
  logs_from = -Inf,
  logs_to = Inf,
  path = NULL
) {
  # local bindings
  name <- NULL

  # initialise empty RO-Crate with audit settings
  crate <- x |>
    dsROCrate::init(
      profile = profile,
      project = project,
      resources = resources,
      tables = tables,
      user = user,
      path = path
    )

  # if `project` is missing, then error
  if (is.null(project)) {
    stop("A `project` name is required!", call. = FALSE)
  }

  # extract list with all projects to verify `project` contains a valid value
  ds <- backend_projects(x)
  server_prjs <- ds[, "name"]
  idx <- project %in% server_prjs
  if (!all(idx)) {
    stop(
      "The following project",
      ifelse(length(idx) == 1, " is ", "s are "),
      "not valid: \n",
      paste0(" - ", project[!idx], collapse = "\n"),
      call. = FALSE
    )
  }

  # Safe People ----
  # get users' details
  safe_people_tbl <- filter_safe_people(x)
  if (!is.null(user)) {
    safe_people_tbl <- safe_people_tbl |>
      dplyr::filter(tolower(name) %in% user)
  }

  # an audit report is not meaningful without Safe People details, whether
  # that's because a `user` filter matched nobody, or because the
  # permission lookup itself could not be completed
  if (nrow(safe_people_tbl) == 0) {
    stop(
      if (is.null(user)) {
        paste(
          "No Safe People details could be found for this project - the",
          "audit cannot proceed."
        )
      } else {
        sprintf(
          "No Safe People details were found for the user: %s!",
          paste0("'", user, "'", collapse = ", ")
        )
      },
      call. = FALSE
    )
  }

  crate <- safe_people_tbl$name |>
    purrr::reduce(
      \(crate, u) {
        safe_people(
          crate,
          connection = x,
          user = u,
          set_author = FALSE,
          set_project = FALSE
        )
      },
      .init = crate
    )

  # Safe Projects ----
  crate <- project |>
    purrr::reduce(
      \(crate, p) {
        safe_project(crate, connection = x, project = p)
      },
      .init = crate
    )

  # Safe Data ----
  crate <- project |>
    purrr::reduce(
      \(crate, p) {
        safe_data(crate, connection = x, project = p)
      },
      .init = crate
    )

  # remove permissions associated with admin users
  non_admin_user_ids <- safe_people_tbl$name |>
    purrr::map_chr(id_hash, prefix = "#person:")
  admin_perm_ents <- crate$`@graph` |>
    purrr::keep(\(x) grepl("^#perm:", getElement(x, "@id"))) |>
    purrr::discard(\(x) getElement(x, "agent")[[1]] %in% non_admin_user_ids)
  crate <- admin_perm_ents |>
    purrr::reduce(rocrateR::remove_entity, .init = crate)

  # Safe Settings ----
  crate <- safe_setting(crate, connection = x)

  # Safe Outputs ----
  crate <- safe_people_tbl$name |>
    purrr::reduce(
      \(crate, u) {
        safe_output(
          crate,
          connection = x,
          path = path,
          user = u,
          logs_to = logs_to,
          logs_from = logs_from
        )
      },
      .init = crate
    )
}

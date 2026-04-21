#' Audit Engine
#'
#' Internal function to create audits for various back-ends.
#'
#' @param x This can be a connection to a 'DataSHIELD' server (e.g., object with
#'     the `opal` class, see [opalr::opal.login()]). Alternatively, a governance
#'     archive file, representing the intent of a project and associated
#'     governance details.
#' @param ... Other optional arguments, see full documentation for details.
#' @param project String with project name(s) from which to extra Safe Project
#'     details.
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
audit_engine <- function(x, ...) {
  UseMethod("audit_engine")
}

#' @rdname audit_engine
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

#' @rdname audit_engine
#' @export
audit_engine.opal <- function(
  x,
  ...,
  project = NULL,
  user = NULL,
  logs_from = -Inf,
  logs_to = Inf,
  path = NULL
) {
  # local bindings
  name <- principal <- NULL

  # create RO-Create with the 5 safes profile
  crate <- rocrateR::rocrate_5s()

  # validate Opal connection
  is_opal_admin_con(x)

  # if `project` is missing, then ~extract all project names~ error
  if (is.null(project)) {
    stop("A `project` name is required!", call. = FALSE)
  }

  # extract all data sources to verify `project` contains a valid value.
  ds <- opalr::opal.datasources(x)
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
  safe_people_tbl <- opalr::oadmin.user_profiles(x, df = FALSE) |>
    dplyr::bind_rows() |>
    dplyr::rename(name = principal) |>
    # exclude system administrators from the report
    dplyr::filter(!(tolower(name) %in% c("admin", "administrator")))

  if (!is.null(user)) {
    safe_people_tbl <- safe_people_tbl |>
      dplyr::filter(tolower(name) %in% user)

    if (nrow(safe_people_tbl) == 0) {
      stop(
        sprintf(
          "No Safe People details were found for the user: %s!",
          paste0("'", user, "'", collapse = ", ")
        ),
        call. = FALSE
      )
    }
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
  crate <- safe_setting(x, rocrate = crate)

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

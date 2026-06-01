#' Safe Data details
#'
#' Safe Data details for the RO-Crate.
#'
#' Researchers only use de-identified data that is relevant to their study.
#'
#' In compliance with the Digital Economy Act, data is effectively anonymised
#' within TREs (Trusted Research Environments).
#'
#' This means any sensitive information that might lead to an individual being
#' identified, such as names and addresses, is either removed or replaced with
#' a random code. Researchers are not processing personal data when using data
#' prepared in this way and when the other Safes are in place. Find out more
#' about de-identification:
#' <https://www.researchdata.scot/engage-and-learn/data-explainers/what-is-data-de-identification/>
#'
#' @param x This can be a connection to a 'DataSHIELD' server (e.g., object with
#'     the `opal` class, see [opalr::opal.login()]), an RO-Crate
#'     ([rocrate][rocrateR::rocrate()] class) or a string with the path to an
#'     RO-Crate.
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::safe_data`][safe_data()].
#' @param include Vector of strings with types of assets to be included, either
#'     `"resources"`, `"tables"` or both.
#' @param asset_id_suffix String with ID suffix for the tables/datasets
#'     entities in the RO-Crate (default: `"#asset:"`).
#' @param project_id_suffix String with ID suffix for the project entities
#'     in the RO-Crate (default: `"#project:"`).
#' @inheritParams init
#'
#' @returns Updated RO-Crate object with Safe Data information.
#' @export
#'
#' @source
#' \itemize{
#'  \item Research Data Scotland, 2025. "What is the Five Safes framework?".
#'  <https://www.researchdata.scot/engage-and-learn/data-explainers/what-is-the-five-safes-framework/>
#' }
safe_data <- function(x, ...) {
  UseMethod("safe_data")
}

#' @export
safe_data.default <- function(x, ...) {
  stop(
    "Unknown class, please try either a file path or",
    " an object with `rocrate` class!"
  )
}

#' @rdname safe_data
#' @export
safe_data.character <- function(
  x,
  ...,
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables"),
  asset_id_suffix = "#asset:",
  project_id_suffix = "#project:",
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  user = attr(x, "user")
) {
  # attempt loading the RO-Crate
  rocrate <- rocrateR::load_rocrate(x)

  # call method with given `rocrate` object:
  safe_data(
    rocrate,
    connection = connection,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables,
    asset_id_suffix = asset_id_suffix,
    project_id_suffix = project_id_suffix,
    path = path,
    user = user
  )
}

#' @rdname safe_data
#' @export
safe_data.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s(),
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL,
  include = c("tables", "resources"),
  asset_id_suffix = "#asset:",
  project_id_suffix = "#project:",
  path = NULL,
  user = NULL
) {
  # declare local bindings
  created <- lastUpdate <- name <- new_dataset_entity <- subject <- NULL

  # validate backend
  validate_backend(x, ...)

  # match the assets to be included
  include <- match.arg(include, several.ok = TRUE)

  # enforce that `project` is a single value
  if (is.null(project)) {
    stop("A value for `project` is required!", call. = FALSE)
  } else if (length(project) != 1) {
    stop("`project` must be a single value!", call. = FALSE)
  }

  project_id <- id_hash(project_id_suffix, project)

  # extract assets for the given project
  all_assets <- list()

  # ---- tables ----
  if ("tables" %in% include) {
    tbl_assets <- get_project_assets(x, project, "tables")
    if (!is.null(tbl_assets)) {
      if (!is.null(tables)) {
        tbl_assets <- dplyr::filter(tbl_assets, name %in% tables)
      }
      all_assets$tables <- tbl_assets
    }
  }

  # ---- resources ----
  if ("resources" %in% include) {
    res_assets <- get_project_assets(x, project, "resources")
    if (!is.null(res_assets)) {
      if (!is.null(resources)) {
        res_assets <- dplyr::filter(res_assets, name %in% resources)
      }
      all_assets$resources <- res_assets
    }
  }

  # combine assets
  assets_tbl <- dplyr::bind_rows(all_assets)

  # check that project assets were found
  err_msg <- sprintf("No details were found for `project = '%s'!", project)
  if (is.null(assets_tbl) || nrow(assets_tbl) == 0) {
    stop(err_msg, call. = FALSE)
  }

  # build asset entities ----
  asset_entities <- build_asset_entities(
    assets_tbl,
    project_id = project_id,
    asset_id_suffix = asset_id_suffix
  )

  # link asset entities with project entity
  rocrate <- link_assets_to_project(
    rocrate,
    project_id,
    vapply(asset_entities, `[[`, "", "@id")
  )

  # build ID lookup ----
  id_lookup <- stats::setNames(
    vapply(asset_entities, `[[`, "", "@id"),
    assets_tbl$name
  )

  # lineage ----
  lineage_df <- infer_table_resource_lineage(assets_tbl)

  rocrate <- add_lineage_relations(
    rocrate,
    lineage_df,
    id_lookup
  )

  # permissions ----
  rocrate <- add_asset_permissions_to_crate(
    rocrate,
    x,
    project,
    assets_tbl,
    id_lookup
  )

  # add asset entities to the RO-Crate
  rocrate <- purrr::reduce(
    asset_entities,
    rocrateR::add_entity,
    .init = rocrate,
    overwrite = TRUE
  )

  # attach input arguments as attributes
  attr(rocrate, "connection") <- x
  attr(rocrate, "path") <- path
  attr(rocrate, "profile") <- profile
  attr(rocrate, "project") <- project
  attr(rocrate, "resources") <- resources
  attr(rocrate, "tables") <- tables
  attr(rocrate, "user") <- user

  # return RO-Crate with the new entity
  return(rocrate)
}

#' @rdname safe_data
#' @export
safe_data.rocrate <- function(
  x,
  ...,
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables"),
  asset_id_suffix = "#asset:",
  project_id_suffix = "#project:",
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  user = attr(x, "user")
) {
  # check if the connection was given
  if (is.null(connection)) {
    stop("A `connection` object is required!", call. = FALSE)
  }

  # call method with given `connection` object:
  safe_data(
    connection,
    rocrate = x,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables,
    asset_id_suffix = asset_id_suffix,
    project_id_suffix = project_id_suffix,
    path = path,
    user = user
  )
}

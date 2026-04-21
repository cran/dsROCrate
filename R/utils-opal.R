#' Add Asset Permissions to RO-Crate
#'
#' Creates a Permission entity describing dataset-level access for a person
#' within a project and links it to the relevant assets.
#'
#' @param rocrate RO-Crate object, see [rocrateR::rocrate].
#' @param project Project identifier.
#' @param person User identifier.
#' @param assets Vector of strings with asset identifiers.
#'
#' @return Updated RO-Crate.
#'
#' @noRd
add_asset_permissions_to_crate <- function(
  rocrate,
  x,
  project,
  assets_tbl,
  id_lookup
) {
  for (i in seq_len(nrow(assets_tbl))) {
    asset <- assets_tbl[i, ]

    asset_id <- id_lookup[[asset$name]]
    asset_type <- asset$asset_type

    # retrieve asset permissions
    perms <- get_asset_permissions(x, project, asset_type, asset$name)

    if (is.null(perms)) {
      next
    }

    # iterate through person permissions for an asset
    for (j in seq_len(nrow(perms))) {
      person <- perms$person[j]
      permission <- perms$permission[j]

      person_id <- id_hash("#person:", person)

      # create permission entities
      perm_ents <- user_asset_perm_entities(
        person = person,
        person_id = person_id,
        asset = asset$name,
        asset_id = asset_id,
        permission = permission,
        asset_type = asset_type
      )

      # add permission entities
      rocrate <- purrr::reduce(
        perm_ents,
        rocrateR::add_entity,
        overwrite = TRUE,
        .init = rocrate
      )
    }
  }

  rocrate
}

#' @noRd
add_lineage_relations <- function(rocrate, lineage_df, id_lookup) {
  if (is.null(lineage_df) || nrow(lineage_df) == 0) {
    return(rocrate)
  }

  for (i in seq_len(nrow(lineage_df))) {
    tbl_id <- id_lookup[[lineage_df$table_name[i]]]
    res_id <- id_lookup[[lineage_df$resource_name[i]]]

    tbl_ent <- rocrate$graph[[tbl_id]]

    tbl_ent$isBasedOn <- list(`@id` = res_id)

    rocrate$graph[[tbl_id]] <- tbl_ent
  }

  rocrate
}

#' Build entities for project assets
#'
#' @param assets_tbl Tibble with project assets (see `get_project_assets()`).
#' @param project_id String with project `@id`.
#' @param asset_id_suffix String with asset `@id` suffix.
#'
#' @returns List with entities for the project assets.
#' @noRd
build_asset_entities <- function(assets_tbl, project_id, asset_id_suffix) {
  purrr::pmap(
    assets_tbl,
    function(
      asset_type,
      project,
      name,
      description,
      created,
      updated,
      url,
      meta
    ) {
      rocrateR::entity(
        id = id_hash(asset_id_suffix, paste0(project, name)),
        type = map_asset_type(asset_type, meta, url),
        name = name,
        description = safe_desc(description, name, asset_type),
        url = url,
        dateCreated = safe_time(created),
        dateModified = safe_time(updated),
        isPartOf = list(`@id` = project_id) #,
        # extra = list(
        #   assetKind = asset_type,
        #   backend = meta
        # )
      )
    }
  )
}

#' Get project's asset permissions
#'
#' @param x Connection to OBiBa's Opal server (see [opalr::opal.login()]).
#' @param project String with project name.
#' @param asset_type String with type of asset, either `tables` or `resources`.
#' @param name String with asset name.
#'
#' @returns Tibble with project asset permissions
#' @noRd
get_asset_permissions <- function(x, project, asset_type, name) {
  if (asset_type == "table") {
    perms <- opalr::opal.table_perm(x, project, name)
  } else {
    perms <- opalr::opal.resource_perm(x, project, name)
  }

  if (is.null(perms) || length(perms$subject) == 0) {
    return(NULL)
  }

  tibble::tibble(
    person = perms$subject,
    permission = perms$permission
  )
}

#' Get project assets.
#'
#' @param x Connection to OBiBa's Opal server (see [opalr::opal.login()]).
#' @param project String with project name.
#' @param type Type of assets to extract, either `tables` or `resources`.
#'
#' @returns Tibble with project assets details.
#' @noRd
get_project_assets <- function(x, project, type = c("tables", "resources")) {
  type <- match.arg(type)

  # verify if project exists
  project_exists(x, project = project)

  if (type == "tables") {
    prj <- opalr::opal.tables(x, project, df = FALSE)

    if (length(prj) == 0) {
      return(NULL)
    }

    tibble::tibble(
      asset_type = "table",
      project = project,
      name = vapply(prj, \(x) x$name %||% "", ""),
      description = vapply(prj, \(x) x$description %||% "", ""),
      created = vapply(prj, \(x) safe_time(x$timestamps$created), character(1)),
      #   prj,
      #   \(x) as.POSIXct(x$timestamps$created, tz = "UTC") %||% NA_real_,
      #   character(1)
      # ),
      updated = vapply(
        prj,
        \(x) safe_time(x$timestamps$lastUpdate),
        character(1)
      ),
      #   prj,
      #   \(x) as.POSIXct(x$timestamps$lastUpdate, tz = "UTC") %||% NA_real_,
      #   character(1)
      # ),
      url = vapply(prj, \(x) x$link %||% NA_character_, ""),
      meta = vector("list", length(prj))
    )
  } else if (type == "resources") {
    res <- opalr::opal.resources(x, project, df = FALSE)

    if (length(res) == 0) {
      return(NULL)
    }

    tibble::tibble(
      asset_type = "resource",
      project = project,
      name = vapply(res, \(x) x$name %||% "", ""),
      description = vapply(res, \(x) x$description %||% "", ""),
      created = vapply(res, \(x) safe_time(x$created), character(1)),
      #   res,
      #   \(x) as.POSIXct(x$created, tz = "UTC") %||% NA_real_,
      #   character(1)
      # ),
      updated = vapply(res, \(x) safe_time(x$updated), character(1)),
      #   res,
      #   \(x) as.POSIXct(x$updated, tz = "UTC") %||% NA_real_,
      #   character(1)
      # ),
      url = vapply(res, function(x) x$resource$url %||% NA_character_, ""),
      meta = parse_resource_params(
        vapply(res, `[[`, "", "parameters")
      )
    )
  }
}

#' Get project details (including tables)
#'
#' @inheritParams get_table_permissions
#'
#' @returns Data frame with project and tables associated
#' @keywords internal
#'
#' @family Opal
get_project_details <- function(x, project) {
  project |>
    lapply(function(p) {
      tryCatch(
        {
          # check if the given project exists
          project_exists(x, project = p)

          # retrieve tables for the given project
          project_tables <- get_project_tables(x, p)
          # check if any tables were found attached to the project
          if (length(project_tables) < 1) {
            return(tibble::tibble(project = p, table = NA))
          }
          tibble::tibble(
            project = p,
            table = project_tables
          )
        },
        error = function(e) {
          return(tibble::tibble(project = p, table = NA))
        }
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(table))
}

#' Get project tables
#'
#' Wrapper for [opalr::opal.project()].
#'
#' @inheritParams get_table_permissions
#'
#' @returns List of project tables
#' @keywords internal
#'
#' @family Opal
get_project_tables <- function(x, project) {
  # verify if project exists
  project_exists(x, project = project)

  # extract table names associated to `project`
  project_tables <- opalr::opal.project(x, project) |>
    getElement("datasource") |>
    getElement("table") |>
    unlist()

  # verify if `project_tables` is missing or NULL, if so, print warning message
  if (all(is.na(project_tables)) || all(is.null(project_tables))) {
    warning(
      "The given `project`, does not have any tables associated!",
      call. = FALSE
    )

    # return empty list, invisibly
    return(invisible(list()))
  }

  # return project tables
  return(project_tables)
}

#' Get table permissions
#'
#' Wrapper for the [opalr::opal.table_perm()] function.
#'
#' @inheritParams validate_opal_con
#' @param project String with project name.
#' @param tables String (or vector of strings) with table names for the given
#'     project.
#'
#' @returns Data frame with permissions for each table in `tables`.
#' @keywords internal
#'
#' @family Opal
get_table_permissions <- function(x, project, tables) {
  seq_along(tables) |>
    lapply(function(j) {
      tryCatch(
        {
          tibble::tibble(
            project = project,
            table = tables[j],
            # get permissions for each dataset inside each project
            opalr::opal.table_perm(x, project, tables[j])
          )
        },
        error = function(e) {
          if (grepl("HTTP 403", e$message)) {
            stop(
              "The provided connection does not have access to retrieve ",
              "table permissions!",
              call. = FALSE
            )
          } else if (grepl("HTTP 404", e$message)) {
            warning(
              "Error when retrieving permissions for ",
              paste0(project, ".", tables[j], "!"),
              call. = FALSE
            )
          } else {
            warning(e$message, call. = FALSE)
          }

          # return empty tibble, only with `project` and `table` details
          return(tibble::tibble(
            project = project,
            table = tables[j]
          ))
        }
      )
    }) |>
    # combine results from the permissions for each table
    dplyr::bind_rows()
}

#' Flatten user permission entities
#'
#' @param x List with entities, generated by [user_perm_entity()].
#'
#' @returns Tibble with properties of the entities.
#' @keywords internal
flatten_user_perm_entity <- function(x) {
  # local bindings
  agent <- object <- asset_id <- person_id <- NULL

  # flatten list of entities
  x_tbl <- x |>
    lapply(tibble::as_tibble, .name_repair = "minimal") |>
    dplyr::bind_rows()
  # check there were any entities found in the previous step
  if (nrow(x_tbl) == 0) {
    return(NULL)
  }
  # edit tibble with entities
  x_tbl |>
    dplyr::rename(
      perm_id = "@id",
      type = "@type",
      person_id = agent,
      asset_id = object
    ) |>
    dplyr::mutate(
      person_id = unlist(person_id),
      asset_id = unlist(asset_id),
    ) |>
    # add permission field, based on `type`
    dplyr::mutate(
      permission = dplyr::case_when(
        type == "ReadAction" ~ "read",
        type == "WriteAction" ~ "write",
        type == "ControlAction" ~ "administrate",
        TRUE ~ NA_character_
      )
    )
}

#' @noRd
infer_table_resource_lineage <- function(assets_tbl) {
  # local binding
  asset_type <- NULL

  tables <- dplyr::filter(assets_tbl, asset_type == "table")
  resources <- dplyr::filter(assets_tbl, asset_type == "resource")

  if (nrow(tables) == 0 || nrow(resources) == 0) {
    return(NULL)
  }

  purrr::map(seq_len(nrow(tables)), function(i) {
    tbl <- tables[i, ]

    matches <- purrr::map(resources$meta, function(m) {
      !is.null(m$table) && identical(m$table, tbl$name)
    }) |>
      purrr::list_c()

    if (!any(matches)) {
      return(NULL)
    }

    tibble::tibble(
      table_name = tbl$name,
      resource_name = resources$name[matches]
    )
  }) |>
    purrr::list_c()
}

#' Verify if connection was created by an administrative user
#'
#' @inheritParams validate_opal_con
#'
#' @returns Boolean flag to indicate whether the given connection was created
#'     by an administrative user.
#' @keywords internal
is_opal_admin_con <- function(x) {
  # validate connection
  validate_opal_con(x)

  # extract the user profile, `uprofile` from the connection object
  uprofile <- getElement(x, "uprofile")

  # extract logged in user's groups
  groups <- getElement(uprofile, "groups") |>
    sapply(unlist)

  # check if the user has admin in their groups
  if ("admin" %in% groups) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @noRd
link_assets_to_project <- function(rocrate, project_id, asset_ids) {
  proj_entity <- .get_entity(rocrate, id = project_id)[[1]]

  # check if no project entity was found (safe_data called before safe_project)
  if (is.null(proj_entity)) {
    return(rocrate)
  }

  # if project entity was found, check if it has a `hasPart` value
  if (is.null(proj_entity$hasPart)) {
    proj_entity$hasPart <- list()
  }

  rocrate |>
    rocrateR::add_entity_value(
      id = project_id,
      key = "hasPart",
      value = c(
        proj_entity$hasPart,
        lapply(asset_ids, function(id) list(`@id` = id))
      ),
      overwrite = TRUE
    )
}

#' @noRd
map_asset_type <- function(asset_type, meta, url) {
  if (asset_type == "table") {
    return("Dataset")
  }

  if (asset_type == "resource") {
    if (!is.null(meta$driver)) {
      return("Dataset")
    }
    if (grepl("^file:", url)) {
      return("File")
    }
    return("Dataset")
  }

  "DigitalDocument"
}

normalise_permission <- function(p) {
  allowed <- c("view", "view-values", "edit", "edit-values", "administrate")
  if (p %in% allowed) {
    return(p)
  }
  "view"
}

#' Parse project resources parametres
#'
#' @param params_json JSON object with resource params.
#'
#' @returns list with simplified JSON params.
#'
#' @noRd
parse_resource_params <- function(params_json) {
  purrr::map(params_json, function(p) {
    if (is.na(p) || p == "") {
      return(list())
    }
    jsonlite::fromJSON(p, simplifyVector = TRUE)
  })
}

safe_desc <- function(desc, name, type) {
  desc %||% paste(type, name)
}

safe_time <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(character(1))
  }
  tryCatch(
    format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%SZ"),
    error = function(e) character(1)
  )
}

#' Update datasets linked to a project
#'
#' Update datasets linked to a project (`hasPart`)
#'
#' @inheritParams safe_data
#' @param ds_ids Vector with `@id`s of the datasets to be linked to `project`
#'
#' @returns Update RO-Crate with updated project entity
#' @keywords internal
update_project_datasets <- function(rocrate, project, ds_ids) {
  # attempt to retrieve the project entities to link up the IDs to the project
  # this only valid if safe_project is called before safe_data
  suppressWarnings({
    project_ents <- rocrate |>
      .get_entity(type = "Project") |>
      # only keep entity for `project`
      Filter(f = \(x) getElement(x, "name") == project)
  })

  # if any entity was found, then filter to keep those for which their @id
  # starts with `project_id_suffix` as set by `safe_project()`:
  if (length(project_ents) == 1 && !is.null(project)) {
    # extract the `hasPart` section
    has_part <- project_ents |>
      sapply("[[", "hasPart") |>
      unlist()

    # update the `hasPart` section
    rocrate <- rocrate |>
      rocrateR::add_entity_value(
        id = project_ents[[1]]["@id"],
        key = "hasPart",
        value = c(has_part, ds_ids) |>
          unique() |>
          lapply(\(id) list(`@id` = id)),
        overwrite = TRUE
      )
  }

  # return update RO-Crate
  return(rocrate)
}

#' @noRd
user_asset_perm_entities <- function(
  person,
  person_id,
  asset,
  asset_id,
  permission,
  asset_type = c("table", "resource")
) {
  asset_type <- match.arg(asset_type)

  # reuse existing function by aliasing "table" args
  user_perm_entity(
    person = person,
    person_id = person_id,
    asset = asset,
    asset_id = asset_id,
    permission = permission
  )
}

#' Create user/person permission entities
#'
#' @param person String with person name/username.
#' @param person_id String with person `@id`.
#' @param asset String with dataset/table/resource name.
#' @param asset_id String with dataset/table `@id`.
#' @param permission String with permission ('view', 'view-values', 'edit',
#'     'edit-values' OR 'administrate').
#' @param ... Other additional values.
#'
#' @returns List of [rocrateR::entity] objects
#' @keywords internal
user_perm_entity <- function(
  person,
  person_id,
  asset,
  asset_id,
  permission,
  ...
) {
  # set local bindings
  description <- type <- NULL
  action_status <- "PotentialActionStatus"
  agent <- list(list(`@id` = person_id))
  object <- list(list(`@id` = asset_id))

  # create combined @id
  comb_id <- id_hash("#perm:", paste0(person, "-", asset))

  # update combine @id, @type and description based on permission
  if (permission == "view") {
    comb_id <- paste0(comb_id, "-dict-summary-read")
    type <- "ReadAction"
    description <- "User may view table dictionary and summary statistics only; access to individual values is restricted."
  } else if (permission == "view-values") {
    comb_id <- paste0(comb_id, "-read-all")
    type <- "ReadAction"
    description <- "User may view table dictionary and all individual values."
  } else if (permission == "edit") {
    comb_id <- paste0(comb_id, c("-write-dict", "-summary-read"))
    type <- c("WriteAction", "ReadAction")
    description <- c(
      "User may edit the table dictionary but cannot view individual values.",
      "User may view summary statistics only; access to individual values is restricted."
    )
  } else if (permission == "edit-values") {
    comb_id <- paste0(comb_id, c("-write-dict", "-read-all"))
    type <- c("WriteAction", "ReadAction")
    description <- c(
      "User may edit the table dictionary.",
      "User may view table dictionary and all individual values."
    )
  } else if (permission == "administrate") {
    comb_id <- paste0(comb_id, "-admin-table")
    type <- "ControlAction"
    description <- "User has full administrative rights: view/edit dictionary and view/edit individual values."
  } else {
    return(NULL)
  }

  # create entity objects
  permission_entities_tbl <- tibble::tibble(
    id = comb_id,
    type = type,
    agent = agent,
    object = object,
    actionStatus = action_status,
    description = description
  )

  # seq_len(nrow(permission_entities_tbl)) |>
  #   lapply(function(i) {
  #     rocrateR::entity(permission_entities_tbl$`id`[i],
  #                      type = permission_entities_tbl$type[i])
  #   })
  purrr::pmap(permission_entities_tbl, rocrateR::entity)
  # rocrateR::entity(as.list(permission_entities_tbl))
}

#' Validate OBiBa's Opal connection
#'
#' @param x Connection to OBiBa's Opal server (see [opalr::opal.login()]).
#'
#' @returns Nothing, call for its side effect.
#' @keywords internal
validate_opal_con <- function(x) {
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

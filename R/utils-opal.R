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
  # retrieve users (if any), safe_people.* should be executed first
  safe_people_tbl <- flatten_safe_people(rocrate)

  for (i in seq_len(nrow(assets_tbl))) {
    asset <- assets_tbl[i, ]

    asset_id <- id_lookup[[asset$name]]
    asset_type <- asset$asset_type

    # retrieve asset permissions
    perms <- get_asset_permissions(x, project, asset_type, asset$name)

    if (is.null(perms)) {
      next
    }

    # exclude admin and auditors permissions
    if (nrow(safe_people_tbl)) {
      perms <- perms |>
        dplyr::filter(person %in% safe_people_tbl$name)
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
        isPartOf = list(`@id` = project_id)
      )
    }
  )
}

#' Get project's asset permissions
#'
#' @param x Connection to DataSHIELD server (e.g., OBiBa's Opal server).
#' @param project String with project name.
#' @param asset_type String with type of asset, either `tables` or `resources`.
#' @param name String with asset name.
#'
#' @returns Tibble with project asset permissions
#' @noRd
get_asset_permissions <- function(x, project, asset_type, name) {
  if (asset_type == "table") {
    perms <- backend_table_perms(x, project, name)
  } else {
    perms <- backend_resource_perms(x, project, name)
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
#' @param x Connection to DataSHIELD server (e.g., OBiBa's Opal server).
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
    prj <- backend_tables(x, project, df = FALSE)

    if (length(prj) == 0) {
      return(NULL)
    }

    tibble::tibble(
      asset_type = "table",
      project = project,
      name = vapply(prj, \(x) x$name %||% "", ""),
      description = vapply(prj, \(x) x$description %||% "", ""),
      created = vapply(prj, \(x) safe_time(x$timestamps$created), character(1)),
      updated = vapply(
        prj,
        \(x) safe_time(x$timestamps$lastUpdate),
        character(1)
      ),
      url = vapply(prj, \(x) x$link %||% NA_character_, ""),
      meta = vector("list", length(prj))
    )
  } else if (type == "resources") {
    res <- backend_resources(x, project, df = FALSE)

    if (length(res) == 0) {
      return(NULL)
    }

    tibble::tibble(
      asset_type = "resource",
      project = project,
      name = vapply(res, \(x) x$name %||% "", ""),
      description = vapply(res, \(x) x$description %||% "", ""),
      created = vapply(res, \(x) safe_time(x$created), character(1)),
      updated = vapply(res, \(x) safe_time(x$updated), character(1)),
      url = vapply(res, function(x) x$resource$url %||% NA_character_, ""),
      meta = parse_resource_params(
        vapply(res, `[[`, "", "parameters")
      )
    )
  }
}

#' Flatten user permission entities
#'
#' @param x List with entities, generated by `user_perm_entity()`.
#'
#' @returns Tibble with properties of the entities.
#' @keywords internal
#' @noRd
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
#' @noRd
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

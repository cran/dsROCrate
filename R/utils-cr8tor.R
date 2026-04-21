#' Add Safe People entities to RO-Crate
#'
#' Creates Person entities and links them to their Projects using
#' the `memberOf` property.
#'
#' @param rc RO-Crate object, see [rocrateR::rocrate].
#' @param people_tbl Tibble with user metadata.
#'   Columns: username, given_name, family_name, email, affiliation.
#' @param membership_tbl Tibble mapping users to projects.
#'   Columns: username, project.
#'
#' @return Updated RO-Crate
#' @noRd
add_safe_people_entities_cr8tor <- function(rc, people_tbl, membership_tbl) {
  # local bindings
  project <- username <- NULL

  for (i in seq_len(nrow(people_tbl))) {
    p <- people_tbl[i, ]
    person_id <- id_hash("#person:", p$username)

    # projects user belongs to
    projs <- membership_tbl |>
      dplyr::filter(username == p$username) |>
      dplyr::pull(project) |>
      unique()

    memberOf <- purrr::map(projs, \(pr) {
      list(`@id` = id_hash("#project:", pr))
    })

    rc <- rc |>
      rocrateR::add_entity(
        rocrateR::entity(
          id = person_id,
          type = "Person",
          name = p$username,
          givenName = p$given_name,
          familyName = p$family_name,
          email = p$email,
          affiliation = p$affiliation,
          memberOf = memberOf
        )
      )
  }

  rc
}

#' Add Safe Project entities
#'
#' Creates Project entities and links datasets using `hasPart`.
#'
#' @param rc RO-Crate object, see [rocrateR::rocrate].
#' @param proj_tbl Tibble with project metadata.
#'   Columns: id, name.
#' @param data_tbl Tibble with dataset metadata.
#'   Columns: project, asset.
#'
#' @return Updated RO-Crate
#' @noRd
add_safe_project_entities_cr8tor <- function(rc, proj_tbl, data_tbl) {
  # local bindings
  asset <- project <- NULL

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

  for (i in seq_len(nrow(proj_tbl))) {
    project_id <- proj_tbl$project_id[i]
    project <- proj_tbl$name[i]

    ds_ids <- data_tbl |>
      dplyr::filter(project == project_id) |>
      dplyr::pull(asset) |>
      unique() |>
      (\(x) id_hash("#asset:", paste0(project, x)))()

    has_part <- purrr::map(ds_ids, \(x) list(`@id` = x))

    rc <- rc |>
      rocrateR::add_entity(
        rocrateR::entity(
          id = id_hash("#project:", project),
          type = "Project",
          name = project,
          dateCreated = now,
          dateModified = now,
          hasPart = has_part
        )
      )
  }

  rc
}

#' Add Safe Dataset entities
#'
#' Creates Dataset entities and links them back to Projects
#' using `isPartOf`.
#'
#' @param rc RO-Crate object, see [rocrateR::rocrate].
#' @param tbl Tibble with dataset metadata.
#'   Columns: project, asset.
#'
#' @return Updated RO-Crate
#' @noRd
add_safe_data_entities_cr8tor <- function(rc, tbl) {
  for (i in seq_len(nrow(tbl))) {
    asset_id <- id_hash("#asset:", paste0(tbl$project[i], tbl$asset[i]))
    project_id <- id_hash("#project:", tbl$project[i])

    rc <- rc |>
      rocrateR::add_entity(
        rocrateR::entity(
          id = asset_id,
          type = "Dataset",
          name = tbl$asset[i],
          isPartOf = list(`@id` = project_id)
        )
      )
  }

  rc
}

#' Add Group entities
#'
#' Represents governance groups as organisations.
#'
#' @param rc RO-Crate object, see [rocrateR::rocrate].
#' @param groups_tbl Tibble with group metadata.
#'   Columns: group_id, description, project.
#'
#' @return Updated RO-Crate
#' @noRd
add_group_entities_cr8tor <- function(rc, groups_tbl) {
  for (i in seq_len(nrow(groups_tbl))) {
    g <- groups_tbl[i, ]
    gid <- id_hash("#group:", g$group_id)

    rc <- rc |>
      rocrateR::add_entity(
        rocrateR::entity(
          id = gid,
          type = "Organization",
          name = g$group_id,
          description = g$description,
          parentOrganization = list(`@id` = id_hash("#project:", g$project))
        )
      )
  }

  rc
}

#' Add Permission entities (Opal-level)
#'
#' Expands user-asset permission matrix into RO-Crate Action entities.
#' Uses `user_perm_entity()` to generate Read/Write/Control actions.
#'
#' @param rc RO-Crate object, see [rocrateR::rocrate].
#' @param perm_expanded_tbl Tibble with user permissions expanded.
#'   Columns: username, asset, permission.
#'
#' @return Updated RO-Crate
#' @noRd
add_permission_entities_cr8tor <- function(rc, perm_expanded_tbl) {
  for (i in seq_len(nrow(perm_expanded_tbl))) {
    row <- perm_expanded_tbl[i, ]

    person_id <- id_hash("#person:", row$username)
    asset_id <- id_hash("#asset:", paste0(row$project, row$asset))

    ents <- user_perm_entity(
      person = row$username,
      person_id = person_id,
      asset = row$asset,
      asset_id = asset_id,
      permission = row$permission
    )

    if (!is.null(ents)) {
      for (ent in ents) {
        rc <- rc |> rocrateR::add_entity(ent)
      }
    }
  }

  rc
}

add_safe_setting_entities_cr8tor <- function(rc, tbl) {
  for (i in seq_len(nrow(tbl))) {
    nm <- names(tbl)[i]

    rc <- rc |>
      rocrateR::add_entity(
        rocrateR::entity(
          id = id_hash("#setting:", nm),
          type = "PropertyValue",
          name = nm,
          value = as.character(tbl[[i]])
        )
      )
  }

  rc
}

add_safe_output_entities_cr8tor <- function(rc, tbl) {
  rc |>
    rocrateR::add_entity(
      rocrateR::entity(
        id = "#output:audit",
        type = "AssessAction",
        name = "cr8tor Compliance Assessment",
        actionStatus = if (tbl$validated) {
          "CompletedActionStatus"
        } else {
          "PotentialActionStatus"
        }
      )
    )
}

#' Convert audit result into RO-Crate
#'
#' @param audit list returned by audit.cr8tor()
#' @return rocrate object
#' @noRd
as_rocrate_audit <- function(audit) {
  rc <- rocrateR::rocrate_5s()

  # Root dataset describing the audit
  rc <- rc |>
    rocrateR::add_entity_value(
      "./",
      "name",
      "cr8tor 5 Safes Audit",
      overwrite = TRUE
    ) |>
    rocrateR::add_entity_value(
      "./",
      "description",
      "Audit report generated from cr8tor archive",
      overwrite = TRUE
    )

  # ---- Safe People ----
  rc <- rc |>
    add_group_entities_cr8tor(audit$groups) |>
    add_safe_people_entities_cr8tor(
      audit$safe_people$users,
      audit$user_projects
    ) |>
    link_people_to_root(audit$safe_people$users$username)

  # ---- Safe Projects ----
  rc <- rc |>
    add_safe_project_entities_cr8tor(
      audit$safe_projects,
      audit$safe_data$assets
    )

  # ---- Safe Data ----
  rc <- rc |>
    add_safe_data_entities_cr8tor(audit$safe_data$assets)

  # ---- Permissions ----
  rc <- rc |>
    add_permission_entities_cr8tor(
      expand_group_permissions_to_users(
        perm_tbl = audit$permissions |>
          dplyr::left_join(
            audit$safe_projects,
            by = c("project" = "project_id")
          ),
        membership_tbl = audit$user_groups,
        data_tbl = audit$safe_data$assets
      ) |>
        dedupe_effective_permissions()
    )

  # ---- Safe Settings ----
  rc <- rc |>
    add_safe_setting_entities_cr8tor(audit$safe_settings)

  # ---- Safe Outputs ----
  rc <- rc |>
    add_safe_output_entities_cr8tor(audit$safe_outputs)

  rc
}


#' Keep strongest permission per user-asset pair
#'
#' @param perm_tbl Tibble with username, asset, permission.
#' @return Deduplicated tibble
#' @noRd
dedupe_effective_permissions <- function(perm_tbl) {
  # local bindings
  asset <- permission <- strength <- username <- NULL
  strength_order <- c(
    "view",
    "view-values",
    "edit",
    "edit-values",
    "administrate"
  )

  perm_tbl |>
    dplyr::mutate(
      strength = match(permission, strength_order)
    ) |>
    dplyr::group_by(username, asset) |>
    dplyr::slice_max(strength, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-strength)
}

#' Expand cr8tor group permissions to user-asset permissions
#'
#' Converts:
#'   Group → Project permissions
#' into:
#'   User → Table permissions
#'
#' @param perm_tbl Tibble from extract_permissions_cr8tor().
#' @param membership_tbl Tibble from extract_user_groups_cr8tor().
#' @param data_tbl Tibble from extract_safe_data_cr8tor()$tables.
#'
#' @return Tibble with username, project, asset, permission
#' @noRd
expand_group_permissions_to_users <- function(
  perm_tbl,
  membership_tbl,
  data_tbl
) {
  # local bindings
  asset <- name <- project <- role <- user <- username <- NULL

  # map cr8tor role to Opal permission
  role_to_permission <- function(role) {
    dplyr::case_when(
      grepl("admin", role, ignore.case = TRUE) ~ "administrate",
      grepl("analyst", role, ignore.case = TRUE) ~ "edit-values",
      grepl("editor", role, ignore.case = TRUE) ~ "edit",
      grepl("viewer", role, ignore.case = TRUE) ~ "view-values",
      TRUE ~ "view"
    )
  }

  # 1. expand group to users
  perm_users <- perm_tbl |>
    dplyr::left_join(
      membership_tbl,
      by = c("group" = "group")
    ) |>
    dplyr::mutate(
      username = dplyr::coalesce(user, username)
    ) |>
    dplyr::filter(!is.na(username))

  # 2. expand project to tables
  perm_tables <- perm_users |>
    dplyr::inner_join(
      data_tbl,
      by = "project"
    )

  # 3. Map roles to permissions
  perm_tables |>
    dplyr::mutate(project = name) |>
    dplyr::transmute(
      username,
      project,
      asset,
      permission = role_to_permission(role)
    ) |>
    dplyr::distinct()
}

#' Extract high-level cr8tor metadata
#'
#' @param bundle cr8tor_bundle
#' @return tibble with crate-level metadata
#' @noRd
extract_cr8tor_metadata <- function(bundle) {
  # root dataset (the crate itself)
  crate <- bundle$rocrate
  ds <- .get_entity(crate, id = "./")[[1]]

  # publisher may be nested
  publisher <- tryCatch(
    ds$publisher[["@id"]],
    error = function(e) NA_character_
  )

  tibble::tibble(
    crate_name = ds$name %||% NA_character_,
    description = ds$description %||% NA_character_,
    date_published = ds$datePublished %||% NA_character_,
    license = ds$license %||% NA_character_,
    publisher = publisher,
    main_project_id = ds$mainEntity[["@id"]] %||% NA_character_,
    rocrate_context = crate$`@context`,
    n_entities = length(crate$`@graph`)
  )
}

#' Extract integrity information
#'
#' BagIt validation handled during load_rocrate()
#'
#' @param bundle cr8tor_bundle
#' @noRd
extract_integrity_cr8tor <- function(bundle) {
  tibble::tibble(
    bagit_validated = TRUE,
    validation_source = "rocrateR::load_rocrate()"
  )
}

#' Extract Safe Data (datasets & tables)
#'
#' @param bundle cr8tor_bundle
#' @return list(tables tibble, n_tables)
#' @noRd
extract_safe_data_cr8tor <- function(bundle) {
  crate <- bundle$rocrate
  ing <- .get_entity(crate, id = "data/cr8-ingress.yaml")[[1]]

  # check if contents of the ingress entity are missing
  if (is.null(ing$content)) {
    return(list(tables = tibble::tibble(), n_tables = 0))
  }

  ing_yaml <- yaml::yaml.load(paste(ing$content[[1]], collapse = "\n"))
  # extract groups and projects
  group_tbl <- extract_groups_cr8tor(bundle)
  proj_tbl <- extract_safe_projects_cr8tor(bundle)

  assets <- ing_yaml$datasets |>
    purrr::map(function(ds) {
      # project name from locations
      yaml_project <- ds$locations[[1]]$opal_project_name %||% NA_character_
      # canonical project id
      project_id <- map_project_name_to_id(yaml_project, proj_tbl)

      purrr::map(ds$tables, function(tbl) {
        tibble::tibble(
          project = project_id,
          dataset = ds$name,
          asset = tbl$name,
          n_cols = length(tbl$columns)
        )
      }) |>
        purrr::list_c()
    }) |>
    purrr::list_c()

  tibble::tibble(
    assets = assets,
    n_assets = nrow(assets)
  )
}

#' Extract Safe Outputs
#'
#' @param bundle cr8tor_bundle
#' @return tibble
#' @noRd
extract_safe_outputs_cr8tor <- function(bundle) {
  acts <- .get_entity(
    bundle$rocrate,
    type = c("AssessAction", "CreateAction")
  )

  ids <- acts |>
    purrr::map(\(x) x[["@id"]]) |>
    purrr::list_c()

  tibble::tibble(
    n_actions = length(ids),
    validated = any(grepl("Validate", ids)),
    signoff = any(grepl("Sign-Off", ids)),
    disclosure = any(grepl("Disclosure", ids)),
    published = any(grepl("Publish", ids))
  )
}

#' Extract Safe People (users)
#'
#' @param bundle cr8tor_bundle
#' @return list(users tibble, n_users)
#' @noRd
extract_safe_people_cr8tor <- function(bundle) {
  # local bindings
  username <- NULL

  user_docs <- bundle$resources[
    grepl("user-.*\\.ya?ml$", names(bundle$resources))
  ]

  users <- purrr::map(user_docs, function(doc) {
    tibble::tibble(
      username = doc$metadata$name %||% NA_character_,
      given_name = doc$spec$given_name %||% NA_character_,
      family_name = doc$spec$family_name %||% NA_character_,
      email = doc$spec$email %||% NA_character_,
      affiliation = doc$spec$affiliation %||% NA_character_,
      enabled = doc$spec$enabled %||% FALSE
    )
  }) |>
    purrr::list_c() |>
    dplyr::distinct(username, .keep_all = TRUE)

  list(users = users, n_users = nrow(users))
}

extract_user_groups_cr8tor <- function(bundle) {
  user_docs <- bundle$resources[
    grepl("user-.*\\.ya?ml$", names(bundle$resources))
  ]

  purrr::map(user_docs, function(doc) {
    username <- doc$spec$username
    groups <- doc$spec$groups #%||% list()

    if (length(groups) == 0) {
      return(NULL)
    }

    purrr::map(groups, function(g) {
      tibble::tibble(
        username = username,
        group = g$value %||% NA_character_
      )
    }) |>
      purrr::list_c()
  }) |>
    purrr::list_c()
}

extract_user_projects_cr8tor <- function(bundle) {
  # local bindings
  description <- group_id <- NULL
  # extract group details
  grp_tbl <- extract_groups_cr8tor(bundle)
  # extract project details
  prj_tbl <- extract_projects_cr8tor(bundle)
  # extract user docs
  user_docs <- bundle$resources[
    grepl("user-.*\\.ya?ml$", names(bundle$resources))
  ]

  purrr::map(user_docs, function(doc) {
    username <- doc$spec$username
    groups <- doc$spec$groups # %||% list()

    if (length(groups) == 0) {
      return(NULL)
    }

    usr_grp_tbl <- purrr::map(groups, function(g) {
      tibble::tibble(
        username = username,
        group_id = g$value
      )
    }) |>
      purrr::list_c()

    # combine user x groups x projects
    usr_grp_tbl |>
      dplyr::left_join(grp_tbl, by = "group_id") |>
      dplyr::select(-description, -group_id) |>
      dplyr::left_join(prj_tbl, by = "project")
  }) |>
    purrr::list_c() |>
    dplyr::distinct()
}

extract_groups_cr8tor <- function(bundle) {
  grp_docs <- bundle$resources[
    grepl("group-.*\\.ya?ml$", names(bundle$resources))
  ]

  purrr::map(grp_docs, function(doc) {
    tibble::tibble(
      group_id = doc$metadata$name,
      description = doc$spec$description %||% NA_character_,
      project = doc$spec$projects %||% NA_character_
    )
  }) |>
    purrr::list_c()
}

#' Extract project permissions
#'
#' @param bundle cr8tor_bundle
#' @return tibble
#' @noRd
extract_permissions_cr8tor <- function(bundle) {
  grp_docs <- bundle$resources[
    grepl("group-.*\\.ya?ml$", names(bundle$resources))
  ]

  # role inference from group name
  role_from_group <- function(group_name) {
    dplyr::case_when(
      grepl("-admin$", group_name) ~ "admin",
      grepl("-analyst$", group_name) ~ "analyst",
      grepl("-editor$", group_name) ~ "editor",
      grepl("-viewer$", group_name) ~ "viewer",
      TRUE ~ "member"
    )
  }

  purrr::map(grp_docs, function(doc) {
    group_name <- doc$metadata$name %||% NA_character_
    role <- role_from_group(group_name)

    projects <- doc$spec$projects %||% NA_character_
    members <- doc$spec$members %||% NA_character_

    # normalise to vectors
    projects <- unlist(projects)
    members <- unlist(members)

    # if no explicit members, return group-level rule
    if (all(is.na(members))) {
      return(tibble::tibble(
        group = group_name,
        project = projects,
        user = NA_character_,
        role = role
      ))
    }

    # expand grid: user × project
    expand.grid(
      group = group_name,
      project = projects,
      user = members,
      role = role,
      stringsAsFactors = FALSE
    ) |>
      tibble::as_tibble()
  }) |>
    purrr::list_c()
}

extract_projects_cr8tor <- function(bundle) {
  prj_docs <- bundle$resources[
    grepl("project-.*\\.ya?ml$", names(bundle$resources))
  ]

  purrr::map(prj_docs, function(doc) {
    tibble::tibble(
      project = doc$metadata$name,
      description = doc$spec$description %||% NA_character_
    )
  }) |>
    purrr::list_c()
}

#' Extract Safe Projects
#'
#' @param bundle cr8tor_bundle
#' @return tibble
#' @noRd
extract_safe_projects_cr8tor <- function(bundle) {
  # RO-Crate identity
  proj <- .get_entity(bundle$rocrate, type = "Project")[[1]]

  # deployment spec
  proj_docs <- bundle$resources[
    grepl(
      sprintf("project-%s.*\\.ya?ml$", proj$identifier),
      names(bundle$resources)
    )
  ]

  tibble::tibble(
    project_id = proj$identifier,
    name = proj$identifier %||% NA_character_,
    description = proj$name %||% NA_character_,
    uuid = proj$`@id` %||% NA_character_,
    deployment_defined = length(proj_docs) > 0
  )
}

#' Extract Safe Settings
#'
#' @param bundle cr8tor_bundle
#' @return tibble
#' @noRd
extract_safe_settings_cr8tor <- function(bundle) {
  has_deployment <- any(grepl("deployment", names(bundle$resources)))

  tibble::tibble(
    deployment_defined = has_deployment,
    environment_type = "Containerised / Kubernetes",
    config_present = !is.null(bundle$config)
  )
}

#' Find BagIt root for an RO-Crate
#'
#' @param path String with path to RO-Crate bag.
#'
#' @returns String with path to RO-Crate bag root (if any).
#' @noRd
find_bagit_root <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)

  # candidate directories: root + all subdirectories
  candidate_dirs <- c(
    path,
    list.dirs(path, recursive = TRUE, full.names = TRUE)
  )

  for (dir in candidate_dirs) {
    if (
      file.exists(file.path(dir, "bagit.txt")) &&
        dir.exists(file.path(dir, "data"))
    ) {
      return(dir)
    }
  }

  return(NULL)
}

link_people_to_root <- function(rc, usernames) {
  authors <- lapply(usernames, \(u) {
    list(`@id` = id_hash("#person:", u))
  })

  rocrateR::add_entity_value(
    rc,
    id = "./",
    key = "author",
    value = authors,
    overwrite = TRUE
  )
}

#' Load cr8tor governance bundle / project archive
#'
#' A cr8tor project archive contains:
#' * bagit/      → RO-Crate metadata layer.
#' * resources/  → deployment & governance YAML specs.
#' * config.toml → platform configuration.
#'
#' @param x Path to cr8tor ZIP archive.
#' @param ... Unused.
#'
#' @return Object of class `cr8tor`.
#' @keywords internal
load_cr8tor_bundle <- function(x, ...) {
  tmp <- tempfile("cr8tor_")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  utils::unzip(x, exdir = tmp)

  # load RO-Crate layer
  rocrate <- rocrateR::load_rocrate(
    x, #file.path(tmp, "bagit"),
    load_content = TRUE,
    ...
  )

  tmp_root <- find_bagit_root(tmp) |>
    dirname()

  # parse resources layer
  res_dir <- file.path(tmp_root, "resources")

  yaml_files <- list.files(
    res_dir,
    pattern = "\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )

  resources <- purrr::map(yaml_files, yaml::read_yaml)
  names(resources) <- sub(paste0("^", res_dir, "/?"), "", yaml_files)

  # parse config
  cfg_path <- file.path(tmp_root, "config.toml")
  config <- if (file.exists(cfg_path)) RcppTOML::parseTOML(cfg_path) else NULL

  structure(
    list(
      rocrate = rocrate,
      resources = resources,
      config = config,
      root = tmp_root
    ),
    class = c("cr8tor", "cr8tor_bundle", "list")
  )
}

map_project_name_to_id <- function(project, proj_tbl) {
  idx <- which(proj_tbl$description == project)
  if (length(idx) == 0) {
    idx <- which(proj_tbl$name == project)
  }
  if (length(idx)) proj_tbl$project_id[idx[1]] else project
}

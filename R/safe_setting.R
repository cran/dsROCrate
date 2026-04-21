#' Safe Setting details
#'
#' Safe Setting details for the RO-Crate.
#'
#' The organisational and technical settings used to access data are designed
#' to minimise the risk of accidental disclosure of data.
#'
#' These settings also prevent the deliberate disclosure of data to others.
#'
#' Physical settings for data access can include locations like
#' [SafePods](https://safepodnetwork.ac.uk/) – secured rooms that use controlled
#' door access, CCTV and secure technology to ensure that sensitive data cannot
#' be mishandled or removed from the Safe Setting. Researchers can analyse the
#' data in these secure rooms, but do not have access to the internet, external
#' devices (such as printers), or any other way of removing protected data from
#' the space.
#'
#' Digital Safe Settings provide secure access to data from a remote location.
#' To be approved for remote data access, researchers will need to prove that
#' their organisation meets physical and IT security standards.
#'
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::safe_setting`][safe_setting()].
#' @inheritParams init
#'
#' @returns Updated RO-Crate object with Safe Settings information.
#' @export
#'
#' @source
#' \itemize{
#'  \item Research Data Scotland, 2025. "What is the Five Safes framework?".
#'  <https://www.researchdata.scot/engage-and-learn/data-explainers/what-is-the-five-safes-framework/>
#' }
safe_setting <- function(x, ...) {
  UseMethod("safe_setting")
}

#' @export
safe_setting.default <- function(x, ...) {
  stop(
    "Unknown class, please try either a file path or",
    " an object with `rocrate` class!"
  )
}

#' @rdname safe_setting
#' @export
safe_setting.character <- function(
  x,
  ...,
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  tables = attr(x, "tables"),
  resources = attr(x, "resources"),
  user = attr(x, "user")
) {
  # attempt loading the RO-Crate
  rocrate <- rocrateR::load_rocrate(x)

  # call method with given `rocrate` object:
  safe_setting(
    rocrate,
    connection = connection,
    path = path,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables,
    user = user
  )
}

#' @export
#' @rdname safe_setting
safe_setting.cr8tor <- function(x, ..., rocrate = rocrateR::rocrate_5s()) {
  # x: parsed CR8TOR resources list

  # technical infrastructure controls ----
  tech_controls <- list(
    rocrateR::entity(
      id = "#control:containerised-runtime",
      type = "CreativeWork",
      name = "Containerised Execution Environment"
    ),
    rocrateR::entity(
      id = "#control:network-isolation",
      type = "CreativeWork",
      name = "Network Isolation Policies"
    ),
    rocrateR::entity(
      id = "#control:role-based-access",
      type = "CreativeWork",
      name = "Role-Based Access Control"
    )
  )

  # physical controls -----
  physical_controls <- list(
    rocrateR::entity(
      id = "#control:secure-hosting",
      type = "CreativeWork",
      name = "Secure Data Centre Hosting"
    )
  )

  # organisational controls ----
  org_controls <- list(
    rocrateR::entity(
      id = "#control:project-governance",
      type = "CreativeWork",
      name = "Project-Level Governance Model"
    )
  )

  safe_root <- rocrateR::entity(
    id = id_hash("#safesetting:", "cr8tor"),
    type = "CreativeWork",
    name = "Safe Setting Controls (CR8TOR Deployment)",
    hasPart = purrr::map(
      c(tech_controls, physical_controls, org_controls) |> purrr::list_c(),
      ~ list("@id" = .x$`@id`)
    )
  )

  all_entities <- purrr::list_c(
    tech_controls,
    physical_controls,
    org_controls,
    list(safe_root)
  )

  purrr::reduce(all_entities, rocrateR::add_entity, .init = rocrate)
}

#' @rdname safe_setting
#' @export
safe_setting.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s(),
  path = NULL,
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL,
  user = NULL
) {
  # local binding
  Package <- NULL

  # validate connection ----
  # x is a valid opal connection object
  validate_opal_con(x)
  # validate that connection user has administrative rights
  is_opal_admin_con(x)

  # validate profile ----
  if (!opalr::dsadmin.profile_exists(x, profile)) {
    stop(
      sprintf("The given profile name, `%s`, does not exist!", profile),
      call. = FALSE
    )
  }

  # statistical disclosure controls ----
  # extract disclosure settings and create `PropertyValue` entities
  disc_setting_entities <- opalr::dsadmin.get_options(x, profile = profile) |>
    tibble::as_tibble() |>
    purrr::pmap(function(name, value, ...) {
      rocrateR::entity(
        id = id_hash("#disc:", paste0(name, value)),
        type = "PropertyValue",
        name = name,
        value = as.character(value)
      )
    })

  disclosure_env <- rocrateR::entity(
    id = id_hash("#env:disclosure_settings:", profile),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    description = sprintf(
      paste(
        "Disclosure control settings extract from the OBiBa Opal server",
        "connection provided, using the profile: '%s'."
      ),
      profile
    ),
    hasPart = purrr::map(disc_setting_entities, ~ list("@id" = .x$`@id`))
  )

  # computational environment ----
  # extract information about R packages installed in the environment
  pkg_tbl <- opalr::dsadmin.package_descriptions(x) |>
    tibble::as_tibble()
  pkg_entities <- pkg_tbl |>
    purrr::pmap(function(Package, Version, Description, Author, ...) {
      # create new entity
      rocrateR::entity(
        id = id_hash("#software:", paste0(Package, Version)),
        type = "SoftwareApplication",
        name = Package,
        version = Version,
        description = Description |>
          gsub(pattern = "[[:space:]]+", replacement = " ") |>
          trimws()
      )
    })

  # Opal server version
  opal_version <- tryCatch(
    {
      x$version
    },
    error = function(e) NA_character_
  )

  opal_entity <- rocrateR::entity(
    id = id_hash("#software:", paste0("opal", opal_version)),
    type = "SoftwareApplication",
    name = "Opal",
    version = opal_version,
    description = paste(
      "Opal is OBiBa's (https://www.obiba.org/) core database application for",
      "epidemiological studies. Participant data, collected by questionnaires,",
      "medical instruments, sensors, administrative databases etc. can be",
      "integrated and stored in a central data repository under a",
      "uniform model."
    )
  )

  # extract version of dsBase / DataSHIELD server
  dsBase_version <- tryCatch(
    pkg_tbl |>
      dplyr::filter(Package == "dsBase") |>
      (\(x) x$Version)(),
    error = function(e) NA_character_
  )

  software_env <- rocrateR::entity(
    id = id_hash("#env:software_stack:", paste0(opal_version, dsBase_version)),
    type = "CreativeWork",
    name = "Approved Analytical Software Environment",
    description = paste(
      "Software packages installed in the controlled Opal/DataSHIELD",
      "environment used for federated analysis."
    ),
    hasPart = purrr::map(
      c(pkg_entities, list(opal_entity)),
      ~ list("@id" = .x$`@id`)
    )
  )

  # technical controls ----
  tech_controls <- list(
    rocrateR::entity(
      id = "#control:output-checking",
      type = "CreativeWork",
      name = "Statistical Disclosure Output Checking",
      description = paste(
        "Automated disclosure control prevents release of",
        "small-cell counts and disclosive statistics."
      )
    ),
    rocrateR::entity(
      id = "#control:server-side-analysis",
      type = "CreativeWork",
      name = "Server-Side Analysis Enforcement",
      description = paste(
        "Raw data never leaves the secure server;",
        "analysis occurs via vetted aggregate functions."
      )
    ),
    rocrateR::entity(
      id = "#control:session-logging",
      type = "CreativeWork",
      name = "Comprehensive Session Logging",
      description = "All analytical actions are logged and auditable."
    )
  )

  # physical controls ----
  physical_controls <- list(
    rocrateR::entity(
      id = "#control:secure-facility",
      type = "CreativeWork",
      name = "Secure Data Facility",
      description = "Access restricted to approved secure premises."
    )
  )

  # organisational controls ----
  org_controls <- list(
    rocrateR::entity(
      id = "#control:access-governance",
      type = "CreativeWork",
      name = "Access Governance Process",
      description = "Data access committee review and approval required."
    )
  )

  # root safe setting entity ----
  safe_setting_root <- rocrateR::entity(
    id = id_hash("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = c(
      list(disclosure_env, software_env),
      tech_controls,
      physical_controls,
      org_controls
    ) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )

  # add entities to RO-Crate
  all_entities <- list(
    disc_setting_entities,
    disclosure_env,
    pkg_entities,
    opal_entity,
    software_env,
    tech_controls,
    physical_controls,
    org_controls,
    safe_setting_root
  )

  rocrate <- purrr::reduce(all_entities, rocrateR::add_entity, .init = rocrate)

  # link Safe Settings with safe Projects
  rocrate <- link_safe_settings_to_projects(rocrate)

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

#' @rdname safe_setting
#' @export
safe_setting.rocrate <- function(
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
  # check if the connection was given
  if (is.null(connection)) {
    stop("A `connection` object is required!", call. = FALSE)
  }

  # call method with given `connection` object:
  safe_setting(
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

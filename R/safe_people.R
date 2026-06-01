#' Safe People details
#'
#' Safe People details for the RO-Crate.
#'
#' Researchers must be accredited and trained before accessing the data that
#' has been prepared for them.
#'
#' The access service provider may require the researcher to sign a statement
#' that they know and understand the regulations of the TRE.
#'
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::safe_people`][safe_people()].
#' @param user_id_suffix String with ID suffix for the tables/datasets
#'     entities in the RO-Crate (default: `"#dataset:"`).
#' @param set_author Boolean flag to indicate if the current user should be
#'     set as the author of the RO-Crate.
#' @param set_project Boolean flag to indicate if any `Project` entities found
#'     in `x` should be linked to the Safe People entity.
#' @inheritParams init
#'
#' @returns Updated RO-Crate object with Safe People information.
#' @export
#'
#' @source
#' \itemize{
#'  \item Research Data Scotland, 2025. "What is the Five Safes framework?".
#'  <https://www.researchdata.scot/engage-and-learn/data-explainers/what-is-the-five-safes-framework/>
#' }
safe_people <- function(x, ...) {
  UseMethod("safe_people")
}

#' @export
safe_people.default <- function(x, ...) {
  stop(
    "Unknown class, please try either a file path or",
    " an object with `rocrate` class!"
  )
}

#' @rdname safe_people
#' @export
safe_people.character <- function(
  x,
  ...,
  user = attr(x, "user"),
  user_id_suffix = "#person:",
  set_author = TRUE,
  set_project = TRUE,
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables")
) {
  # attempt loading the RO-Crate
  rocrate <- rocrateR::load_rocrate(x)

  # call method with given `rocrate` object:
  safe_people(
    rocrate,
    connection = connection,
    user = user,
    user_id_suffix = user_id_suffix,
    set_author = set_author,
    set_project = set_project,
    path = path,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables
  )
}

#' @rdname safe_people
#' @export
safe_people.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s(),
  user = NULL,
  user_id_suffix = "#person:",
  set_author = TRUE,
  set_project = TRUE,
  path = NULL,
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL
) {
  # validate backend
  validate_backend(x, ...)

  # attempt to retrieve project entity
  project_id <- id_hash("#project:", project)
  safe_project_entity <- rocrate |>
    .get_entity(id = project_id, type = "Project")

  # initialise empty user entity
  user_entity <- NULL

  # check if `user` was given
  if (!is.null(user)) {
    # check if user is a list, if so, then use all the elements as part of
    # the user_entity
    if ("list" %in% class(user)) {
      user_entity <- rocrateR::entity(
        id = c(getElement(user, "@id"), getElement(user, "id")),
        type = "Person",
        name = c(getElement(user, "name"), getElement(user, "username")),
        givenName = c(
          getElement(user, "givenName"),
          getElement(user, "firstname")
        ),
        familyName = c(
          getElement(user, "familyName"),
          getElement(user, "surname")
        ),
        affiliation = list(`@id` = c(getElement(user, "affiliation")))
      )
    } else {
      user_entity <- rocrateR::entity(
        id = paste0(user_id_suffix, digest::digest(user)),
        type = "Person",
        name = user
      )
    }
  } else {
    # create basic user entity
    user_entity <- rocrateR::entity(
      id = paste0(user_id_suffix, digest::digest(x$username)),
      type = "Person",
      name = x$username
    )
    user <- x$username
  }

  # extract user information from the connection object
  user_profile_tbl <- parse_user_profiles(x, user = user)

  # check if the `user_profile_tbl` has a `userInfo` column, if so, then
  # attach the fields in this one to the user entity
  if ("userInfo" %in% colnames(user_profile_tbl)) {
    user_info_tbl <- getElement(user_profile_tbl, "userInfo") |>
      dplyr::bind_rows()
    user_info_cols <- colnames(user_info_tbl)
    for (i in seq_along(user_info_cols)) {
      # avoid overwriting existing fields
      if (!(user_info_cols[i] %in% names(user_entity))) {
        user_entity[user_info_cols[i]] <- user_info_tbl[, i]
      }
    }
  }

  # add membership information, if Safe Project was found and set_project = TRUE
  if (length(safe_project_entity) && set_project) {
    user_entity$memberOf <- safe_project_entity |>
      lapply(\(x) list(`@id` = x[["@id"]]))
  }

  # add user to the RO-Crate
  rocrate <- rocrate |>
    rocrateR::add_entity(user_entity, overwrite = TRUE)

  # link new user entity @id to the root (./) author property
  if (set_author) {
    rocrate <- rocrate |>
      rocrateR::add_entity_value(
        id = "./",
        key = "author",
        value = list(`@id` = getElement(user_entity, "@id")),
        overwrite = TRUE
      )
  }

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

#' @rdname safe_people
#' @export
safe_people.rocrate <- function(
  x,
  ...,
  user = attr(x, "user"),
  user_id_suffix = "#person:",
  set_author = TRUE,
  set_project = TRUE,
  connection = attr(x, "connection"),
  path = attr(x, "path"),
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables")
) {
  # check if the connection was given
  if (is.null(connection)) {
    stop("A `connection` object is required!", call. = FALSE)
  }

  # call method with given `connection` object:
  safe_people(
    connection,
    rocrate = x,
    user = user,
    user_id_suffix = user_id_suffix,
    set_author = set_author,
    set_project = set_project,
    path = path,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables
  )
}

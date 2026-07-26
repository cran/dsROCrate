#' Safe Output details
#'
#' Safe Output details for the RO-Crate.
#'
#' All research outputs are checked to ensure individuals cannot be identified
#' even in the public domain.
#'
#' This means the other four Safes no longer apply.
#'
#' Before outputs are released from the TRE, they are checked to make sure it
#' is reasonably unlikely that any individuals can be identified on publication.
#' Compliance with the Digital Economy Act also requires that the TRE applies
#' methods and standards for output checking that are accredited by the UK
#' Statistics Authority.
#'
#' @param ... Other optional arguments. See the full documentation,
#'     [`?dsROCrate::safe_output`][safe_output()].
#' @param logs_to Upper limit timestamp to filter out the outputs generated
#'     (default: `Sys.time()`, current system time).
#' @param logs_from Lower limit timestamp to filter out the outputs generated
#'     (default: `Sys.time() - 24 * 60 ^ 2`, last 24 hours).
#' @inheritParams init
#'
#' @returns Updated RO-Crate object with Safe Outputs information.
#' @export
#'
#' @source
#' \itemize{
#'  \item Research Data Scotland, 2025. "What is the Five Safes framework?".
#'  <https://www.researchdata.scot/engage-and-learn/data-explainers/what-is-the-five-safes-framework/>
#' }
safe_output <- function(x, ...) {
  UseMethod("safe_output")
}

# @rdname safe_output
#' @export
safe_output.default <- function(x, ...) {
  stop(
    "Unknown class, please try either a file path or",
    " an object with `rocrate` class!"
  )
}

#' @rdname safe_output
#' @export
safe_output.ArmadilloCredentials <- function(x, ...) {
  stop(
    "`safe_output()` for the Armadillo backend is not currently implemented!",
    call. = FALSE
  )
}

#' @rdname safe_output
#' @export
safe_output.character <- function(
  x,
  ...,
  path = attr(x, "path"),
  user = attr(x, "user"),
  logs_to = Sys.time(),
  logs_from = logs_to - 24 * 60^2,
  connection = attr(x, "connection"),
  profile = attr(x, "profile"),
  project = attr(x, "project"),
  resources = attr(x, "resources"),
  tables = attr(x, "tables")
) {
  # attempt loading the RO-Crate
  rocrate <- rocrateR::load_rocrate(x)

  # call method with given `rocrate` object:
  safe_output(
    rocrate,
    connection = connection,
    path = path,
    user = user,
    logs_to = logs_to,
    logs_from = logs_from,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables
  )
}

#' @importFrom utils write.csv
#' @rdname safe_output
#' @export
safe_output.opal <- function(
  x,
  ...,
  rocrate = rocrateR::rocrate_5s(),
  path = NULL,
  user = NULL,
  logs_to = Sys.time(),
  logs_from = logs_to - 24 * 60^2,
  profile = "default",
  project = NULL,
  resources = NULL,
  tables = NULL
) {
  # local bindings
  `@timestamp` <- backend <- logger_name <- safe_people_id <- username <- NULL
  ds_action <- ds_eval <- ds_id <- ds_function <- ds_symbol <- ds_table <- NULL
  asset <- action <- is_placeholder <- kind <- symbol_id <- timestamp <- NULL
  expr <- fx <- log_id <- r_cmd <- session <- symbol <- NULL

  # create formatted versions of input dates
  logs_from_is_valid <- FALSE
  if (!is.infinite(logs_from)) {
    logs_from_is_valid <- is_valid_posixct(logs_from)
  } else {
    # assumes that a value of `Inf` is a valid date
    logs_from_is_valid <- TRUE
  }

  logs_to_is_valid <- FALSE
  if (!is.infinite(logs_to)) {
    logs_to_is_valid <- is_valid_posixct(logs_to)
  } else {
    # assumes that a value of `Inf` is a valid date
    logs_to_is_valid <- TRUE
  }

  # display error message if either date is invalid
  if (!all(logs_from_is_valid, logs_to_is_valid)) {
    stop(
      paste0(
        "Invalid date string identified for: ",
        ifelse(logs_from_is_valid, "", "\n  - logs_from"),
        ifelse(logs_to_is_valid, "", "\n  - logs_to")
      ),
      call. = FALSE
    )
  }

  logs_from_formatted <- ifelse(
    is.infinite(logs_from),
    "ALL",
    format(as.POSIXct(logs_from), '%Y-%m-%d %H:%M:%S')
  )
  logs_to_formatted <- ifelse(
    is.infinite(logs_to),
    "ALL",
    format(as.POSIXct(logs_to), '%Y-%m-%d %H:%M:%S')
  )

  # validate backend
  validate_backend(x, ...)

  # verify if `user` is NULL, if so, retrieve information from the RO-crate
  if (is.null(user)) {
    # get `author` section from the root (./) entity
    rocrate_author <- rocrate |>
      .get_entity(id = "./") |>
      lapply(getElement, name = "author") |>
      sapply(\(x) x)

    # extract @id attribute(s)
    if (length(rocrate_author) > 1) {
      safe_people_id <- rocrate_author |>
        sapply(getElement, name = "@id") |>
        sapply(unlist)
    } else {
      safe_people_id <- rocrate_author["@id"][[1]]
    }

    # check if Safe People section wasn't found
    if (is.null(safe_people_id)) {
      warning(
        "Safe People section not found (i.e., no author for root entity) ",
        "in the given RO-Crate. \nEither run `dsROCrate::safe_people()` ",
        "or set `user` when calling `dsROCrate::safe_output()`!",
        call. = FALSE
      )

      # return the input RO-Crate
      return(rocrate)
    }

    # retrieve Safe People entity for the current user
    safe_people_information <- safe_people_id |>
      sapply(\(x) .get_entity(rocrate, id = x))

    # update user
    user <- safe_people_information |>
      sapply(getElement, name = "name") |>
      unique()
  }

  # check if the given value for `user` is a list
  if ("list" %in% class(user)) {
    user <- user |>
      purrr::pluck("name") |>
      unique()
  }

  # check if for any reason multiple users were found
  if (length(user) != 1) {
    warning(
      "Error when retrieving the Safe People section in the given ",
      "RO-Crate. ",
      length(user),
      " entries in the 'author' ",
      "section of root (./) entity were found!",
      call. = FALSE
    )

    # return the input RO-Crate
    return(rocrate)
  }

  # initialise symbol registry
  registry <- symbol_registry()

  # parse logs
  userlogs_tbl <- backend_logs(x) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      `@timestamp` = as.POSIXct(`@timestamp`, format = "%Y-%m-%dT%H:%M:%S")
    ) |>
    # filter logs
    dplyr::filter(`@timestamp` >= logs_from, `@timestamp` <= logs_to) |>
    dplyr::filter(logger_name == "datashield.user") |>
    dplyr::filter(username %in% user)

  userlogs <- NULL
  if (nrow(userlogs_tbl) > 0) {
    userlogs <- userlogs_tbl |>
      with(
        sprintf(
          "[%s][%s]%-12s%s",
          level,
          format(`@timestamp`, "%Y-%m-%dT%H:%M:%S"),
          paste0("[", ds_action, "]"),
          message
        )
      )
  }

  # check if any logs were found in the given time frame
  if (length(userlogs) == 0) {
    warning(
      "No logs were found for the following configuration:",
      "\n User: ",
      user,
      "\n Period: ",
      logs_from_formatted,
      " -- ",
      logs_to_formatted,
      call. = FALSE
    )

    # return the input RO-Crate
    return(rocrate)
  }

  log_filename <- paste0(
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    "-dslogs-",
    user,
    ".log"
  )

  # create new data entity for log file
  log_entity <- rocrateR::entity(
    id = basename(log_filename),
    type = "File",
    dateModified = Sys.time(),
    name = basename(log_filename),
    description = paste0(
      "This file contains the raw logs for the user: `",
      user,
      "` , between: ",
      logs_from_formatted,
      " and ",
      logs_to_formatted
    ),
    encodingFormat = "text/plain"
  )

  # update symbol registry
  ## extract 'ASSIGN' operations from the logs
  userlogs_assign_tbl <- userlogs_tbl |>
    dplyr::filter(ds_action %in% c("ASSIGN"))

  ## reshape the logs into a tibble of `symbols`
  symbols_tbl <- seq_len(nrow(userlogs_assign_tbl)) |>
    purrr::map(function(i) {
      # extract log components
      ds_eval <- getElement(userlogs_assign_tbl[i, ], "ds_eval")
      ds_resource <- getElement(userlogs_assign_tbl[i, ], "ds_resource")
      ds_symbol <- getElement(userlogs_assign_tbl[i, ], "ds_symbol")
      ds_table <- getElement(userlogs_assign_tbl[i, ], "ds_table")

      # evaluate which fields are populated
      is_expr <- !is.null(ds_eval) && !is.na(ds_eval)
      is_resource <- !is.null(ds_resource) && !is.na(ds_resource)
      is_table <- !is.null(ds_table) && !is.na(ds_table)

      tibble::tibble(
        symbol = ds_symbol,
        kind = ifelse(
          is_expr,
          'expression',
          ifelse(
            is_resource,
            'resource',
            ifelse(is_table, 'table', NA_character_)
          )
        ),
        asset = ifelse(
          is_resource,
          ds_resource,
          ifelse(is_table, ds_table, NA_character_)
        ),
        expr = ifelse(is_expr, ds_eval, NA_character_),
        # expr = if (is_expr) str2lang(ds_eval) else NULL,
        created_by = ifelse(
          is_expr,
          'DSI::datashield.assign.expr',
          ifelse(
            is_resource,
            'DSI::datashield.assign.resource',
            ifelse(is_table, 'DSI::datashield.assign.table', NA_character_)
          )
        ),
        created_at = userlogs_assign_tbl$`@timestamp`[[i]],
        user = userlogs_assign_tbl$username[[i]],
        action = userlogs_assign_tbl$ds_action[[i]],
        session = userlogs_assign_tbl$ds_id[[i]]
      )
    }) |>
    purrr::list_c() |>
    dplyr::distinct()

  ## add symbols to registry
  registry <- symbols_tbl |>
    purrr::pmap(safe_symbol) |>
    purrr::reduce(register_symbol, .init = registry)

  # parse aggregate function calls into list of safe_call objects
  calls_lst <- userlogs_tbl |>
    dplyr::filter((ds_action %in% c("AGGREGATE"))) |>
    # dplyr::filter(!is.na(ds_eval)) |>
    purrr::pmap(function(ds_eval, ...) {
      safe_call(ds_eval, ...) |>
        enrich_call(registry = registry)
    })

  # convert list of calls into tibble
  calls_tbl <- purrr::map(calls_lst, \(x) {
    tibble::tibble(
      timestamp = format(x$created_at, '%Y-%m-%dT%H:%M:%S'),
      action = "AGGREGATE",
      user = x$user,
      r_cmd = x$original,
      fx = paste0(x$package, x$namespace, x$fx),
      args = list(x$args),
      symbol = NA,
      table = x$args |>
        purrr::map(function(x) {
          if (!inherits(x, "safe_reference")) {
            return(NA_character_)
          }
          resolve_symbol_asset(x$symbol_id, registry)
        }),
      session = x$session,
      profile = x$profile
    )
  }) |>
    purrr::list_c()

  # combine function calls with symbol's registry
  calls_symbols_tbl <- calls_tbl |>
    dplyr::select(-symbol) |>
    dplyr::mutate(
      args = purrr::map(
        args,
        ~ purrr::imap_dfr(.x, function(arg, nm) {
          if (!inherits(arg, "safe_reference")) {
            tibble::tibble(
              argument = nm,
              value = list(arg),
              symbol_id = NA_character_,
              symbol = NA_character_,
              column = NA_character_
            )
          } else {
            tibble::tibble(
              argument = nm,
              value = list(arg),
              symbol_id = arg$symbol_id,
              symbol = arg$symbol,
              column = arg$column
            )
          }
        })
      )
    ) |>
    (\(x) {
      purrr::map2(
        split(x |> dplyr::select(-args), seq_len(nrow(x))),
        x$args,
        dplyr::bind_cols
      )
    })() |>
    purrr::list_c() |>
    dplyr::left_join(
      registry$symbols,
      by = c("symbol_id" = "id"),
      suffix = c("", "_registry")
    ) |>
    dplyr::mutate(
      asset = dplyr::if_else(
        kind == "expression",
        purrr::map_chr(
          symbol_id,
          resolve_symbol_asset,
          registry = registry
        ),
        asset
      )
    ) |>
    # add column with backend
    dplyr::mutate(backend = "OBiBa's Opal") |>
    # subset columns
    dplyr::select(
      timestamp,
      action,
      user,
      r_cmd,
      fx,
      symbol,
      kind,
      asset,
      expr,
      # table = ds_table,
      session,
      backend
    )

  # extract session details
  session_tbl <- userlogs_tbl |>
    dplyr::filter((ds_action %in% c("OPEN"))) |>
    dplyr::mutate(
      # format timestamp
      timestamp = format(`@timestamp`, '%Y-%m-%dT%H:%M:%S'),
      # attach session ID, `ds_id`, if `ds_action == 'OPEN'`
      ds_eval = paste0("Open session: ", ds_id),
      # set `ds_function = 'DSI::datashield.login'`
      ds_function = "DSI::datashield.login",
      backend = "OBiBa's Opal"
    ) |>
    dplyr::select(
      timestamp,
      action = ds_action,
      user = username,
      r_cmd = ds_eval,
      fx = ds_function,
      session = ds_id,
      backend
    )

  # combine the logs
  userlogs_tbl_maps_evals <- calls_symbols_tbl |>
    dplyr::distinct() |>
    dplyr::mutate(log_id = dplyr::row_number()) |>
    dplyr::bind_rows(session_tbl) |>
    dplyr::arrange(timestamp, log_id) |>
    dplyr::select(-log_id)

  log_maps_filename <- paste0(
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    "-dslogs-",
    user,
    "_mappings.csv"
  )

  # create new data entity for log file
  log_maps_entity <- rocrateR::entity(
    id = basename(log_maps_filename),
    type = "File",
    dateModified = Sys.time(),
    name = basename(log_maps_filename),
    description = "This file contains mappings and evaluated functions",
    encodingFormat = "text/csv"
  )

  # check if a `path` was not provided, then display warning and store contents
  # inside the RO-Crate entity
  if (is.null(path)) {
    warning(
      "A `path` wasn't provided! The logs will be included in the ",
      "RO-Crate object, under the `content` tag!",
      call. = FALSE
    )
    log_entity$content <- list(userlogs)
    log_maps_entity$content <- list(userlogs_tbl_maps_evals)
  } else {
    # validate if the given path is valid
    if (!dir.exists(path)) {
      warning(
        "The given `path` is not valid! The logs will be included in the ",
        "RO-Crate object, under the `content` tag!",
        call. = FALSE
      )
      log_entity$content <- list(userlogs)
      log_maps_entity$content <- list(userlogs_tbl_maps_evals)
    } else {
      writeLines(userlogs, file.path(path, log_filename))
      write.csv(
        userlogs_tbl_maps_evals,
        file.path(path, log_maps_filename),
        row.names = FALSE
      )
    }
  }

  # add entities to the RO-Crate
  rocrate <- rocrate |>
    rocrateR::add_entity(log_entity) |>
    rocrateR::add_entity(log_maps_entity) |>
    rocrateR::add_entity_value(
      id = "./",
      key = "hasPart",
      value = c(
        getElement(.get_entity(rocrate, id = "./"), "hasPart"),
        list(
          list(`@id` = log_entity$`@id`),
          list(`@id` = log_maps_entity$`@id`)
        )
      ),
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

#' @rdname safe_output
#' @export
safe_output.rocrate <- function(
  x,
  ...,
  path = attr(x, "path"),
  user = attr(x, "user"),
  logs_to = Sys.time(),
  logs_from = logs_to - 24 * 60^2,
  connection = attr(x, "connection"),
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
  safe_output(
    connection,
    rocrate = x,
    path = path,
    user = user,
    logs_to = logs_to,
    logs_from = logs_from,
    profile = profile,
    project = project,
    resources = resources,
    tables = tables
  )
}

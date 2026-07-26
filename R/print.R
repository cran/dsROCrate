#' @export
print.cr8tor_bundle <- function(x, ...) {
  msg <- "<cr8tor governance bundle>"

  is_valid_roc <- function(x) {
    inherits(x$rocrate, "rocrate") &&
      rocrateR::is_rocrate(x$rocrate, error = FALSE)
  }

  if (is_valid_roc(x)) {
    msg <- c(msg, " \U2714 Valid RO-Crate")
  } else {
    msg <- c(msg, " \U2716 Invalid RO-Crate")
  }

  # project metadata (safe extraction)
  proj <- tryCatch(
    x$resources[["governance/cr8-governance.yaml"]]$project,
    error = function(e) NULL
  )

  if (!is.null(proj)) {
    msg <- c(
      msg,
      paste0(" Project: ", proj$name, " (", proj$reference, ")")
    )
  }

  # counts
  n_resources <- length(x$resources)
  n_entities <- length(x$rocrate$`@graph` %||% list())
  n_actions <- length(proj$actions %||% list())
  n_users <- length(
    x$resources[["governance/cr8-governance.yaml"]]$users %||% list()
  )

  msg <- c(
    msg,
    " Contents:",
    paste0("  - Entities:  ", n_entities),
    paste0("  - Resources: ", n_resources),
    paste0("  - Actions:   ", n_actions),
    paste0("  - Users:     ", n_users)
  )

  message(paste0(msg, collapse = "\n"))

  invisible(x)
}

#' @export
print.safe_call <- function(x, ...) {
  msg <- "<safe_call>"
  msg <- c(msg, paste("Function:", paste0(x$package, "::", x$fx)))

  if (length(x$args)) {
    msg <- c(msg, "Arguments:")
    msg <- c(
      msg,
      purrr::map2(x$args, names(x$args), function(arg_val, arg_name) {
        if ("safe_symbol" %in% class(arg_val)) {
          paste0("    ", arg_name, " = ", arg_val$symbol, collapse = "\n")
        } else {
          paste0("    ", arg_name, " = ", unlist(arg_val), collapse = "\n")
        }
      })
    )
    # msg <- c(
    #   msg,
    #   paste0("    ", names(x$args), " = ", unlist(x$args), collapse = "\n")
    # )
  }

  message(paste0(msg, collapse = "\n"))

  invisible(x)
}

#' @export
print.safe_symbol <- function(x, ...) {
  msg <- "<safe_symbol>"

  msg <- c(msg, paste("ID     :", x$id))
  msg <- c(msg, paste("Symbol :", x$symbol))
  msg <- c(msg, paste("Kind   :", x$kind))

  if (!is.null(x$asset)) {
    msg <- c(msg, paste("Asset  :", x$asset))
  }

  if (!is.null(x$column)) {
    msg <- c(msg, paste("Column :", x$column))
  }

  if (!is.null(x$parent)) {
    msg <- c(msg, paste("Parent :", x$parent))
  }

  message(paste0(msg, collapse = "\n"))

  invisible(x)
}

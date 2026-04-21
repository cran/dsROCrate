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

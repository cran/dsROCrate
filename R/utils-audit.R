#' Audit intent of study
#'
#' This internal helper is used to audit and object containing details about the
#' 'intent' of a study. Details include server configuration, user credentials,
#' project and associated assets (e.g., tables and resources).
#'
#' @param excluded_args Vector with names of args to be excluded from the main
#'     audit call.
#' @param ... Additional args to be used in the audit process.
#'
#' @inheritParams audit intent
#'
#' @returns List with two audit objects, one for the intent and one for the main
#' @keywords internal
#'
#' @noRd
audit_intent <- function(intent, excluded_args = c("project", "user"), ...) {
  # if `intent` is NOT NULL, audit this object
  intent_audit <- if (!is.null(intent)) {
    audit(intent, ...)
  } else {
    NULL
  }

  # list of additional args
  main_audit_args <- list(...)

  # check if `intent_audit` is not NULL
  if (!is.null(intent_audit)) {
    # extract Project(s) and People from the `intent` audit crate
    safe_project_tbl <- intent_audit |>
      flatten_safe_project()
    safe_people_tbl <- intent_audit |>
      flatten_safe_people()

    main_audit_args <- exclude_args(main_audit_args, excluded = excluded_args)

    main_audit_args <- c(
      main_audit_args,
      project = safe_project_tbl$project,
      user = safe_people_tbl$name
    )
  }

  list(intent_audit = intent_audit, main_audit_args = main_audit_args)
}

#' Excluded arguments from a list
#'
#' @param ... List with arguments.
#' @param excluded Vector with names of args to be excluded from the input list.
#'
#' @returns List with arguments after filtering the values in `excluded`.
#' @keywords internal
#'
#' @noRd
exclude_args <- function(..., excluded) {
  # capture additional args
  args <- list(...)
  arg_names <- names(args)
  # exclude args that shouldn't be passed to the next function
  args[!(arg_names %in% excluded)]
}

#' Wrapper for [tryCatch]
#'
#' @param expr R code to be executed inside `tryCatch`.
#' @param error_val Value to be returned on failure.
#'
#' @returns List with value and any resulting error.
#' @noRd
.try_load <- function(expr, error_val = NULL) {
  tryCatch(
    list(value = expr, error = NULL),
    error = function(e) list(value = error_val, error = conditionMessage(e))
  )
}

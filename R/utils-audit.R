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

exclude_args <- function(..., excluded) {
  # capture additional args
  args <- list(...)
  arg_names <- names(args)
  # exclude args that shouldn't be passed to the next function
  args[!(arg_names %in% excluded)]
}

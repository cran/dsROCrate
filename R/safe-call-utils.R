#' Enrich argument from function call
#'
#' @param arg String with argument.
#' @param registry Symbol registry object.
#' @param timestamp Timestamp to map the symbol details.
#' @param session String with session ID.
#'
#' @returns `safe_reference` object.
#' @keywords internal
#'
#' @noRd
enrich_argument <- function(arg, registry, timestamp, session) {
  if (!is.character(arg) || length(arg) != 1) {
    return(arg)
  }

  symbol <- resolve_symbol(
    registry,
    symbol = arg,
    timestamp = timestamp,
    session = session
  )

  if (is.null(symbol)) {
    return(arg)
  }

  safe_reference(
    symbol = symbol$symbol,
    symbol_id = symbol$id
  )
}

#' Enrich function call
#'
#' @param call Function call.
#' @param registry Symbol registry object.
#'
#' @returns Updated function call with enrich arguments.
#' @keywords internal
#'
#' @noRd
enrich_call <- function(call, registry) {
  # call `enrich_argument` for each argument in the function call
  call$args <- purrr::map(
    call$args,
    enrich_argument,
    registry = registry,
    timestamp = call$created_at,
    session = call$session
  )

  call
}

#' Get function details
#'
#' @param info List with details for function call.
#'
#' @returns Function invoked in function call.
#' @keywords internal
#'
#' @noRd
get_function <- function(info) {
  if (is.null(info$package)) {
    return(
      tryCatch(get(info$fx, mode = "function"), error = function(e) NULL)
    )
  }

  tryCatch(
    get(info$fx, envir = asNamespace(info$package), mode = "function"),
    error = function(e) NULL
  )
}

#' Parse arguments from a function call
#'
#' @param fx_call R function call.
#' @param info List with details from the function call.
#' @param expand.dots Boolean flag. Should arguments matching `...` in the call
#'     be included or left as a `...` argument?
#'
#' @returns List with simplified arguments.
#' @keywords internal
#'
#' @noRd
parse_arguments <- function(fx_call, info, expand.dots = FALSE) {
  supplied <- as.list(fx_call[-1])
  supplied_names <- names(supplied)

  # (attempt to) recover function object
  fun_obj <- get_function(info)

  matched <- tryCatch(
    if (is.null(fun_obj)) {
      NULL
    } else {
      match.call(
        definition = fun_obj,
        call = fx_call,
        expand.dots = expand.dots
      )
    },
    error = function(e) NULL
  )

  if (is.null(matched)) {
    # fall back to the supplied arguments if matching failed
    matched <- supplied

    # attach arg names
    if (is.null(supplied_names)) {
      supplied_names <- rep("", length(supplied))
    }
    missing <- supplied_names == ""

    supplied_names[missing] <- paste0("..", which(missing))

    names(matched) <- supplied_names
  } else {
    matched <- as.list(matched[-1])
  }

  # evaluate constants only
  matched <- lapply(matched, simplify_argument)

  matched
}

#' Parse function call
#'
#' @param fx_call R function call.
#'
#' @returns List with properties extracted from `fx_call`.
#' @keywords internal
#'
#' @noRd
parse_call <- function(fx_call) {
  fx <- fx_call[[1]]

  # package + function
  info <- parse_function(fx)

  # arguments
  args <- parse_arguments(fx_call, info)

  list(
    original = paste(deparse(fx_call), collapse = ""),
    package = info$package,
    namespace = info$namespace,
    fx = info$fx,
    args = args
  )
}

#' Parse function
#'
#' @param fx R function call.
#'
#' @returns List with function properties
#' @keywords internal
#'
#' @noRd
parse_function <- function(fx) {
  if (is.call(fx) && identical(fx[[1]], as.name("::"))) {
    return(
      list(
        package = as.character(fx[[2]]),
        namespace = "::",
        fx = as.character(fx[[3]])
      )
    )
  }

  list(
    package = NULL,
    namespace = NULL,
    fx = as.character(fx)
  )
}

#' Simplify argument
#'
#' @param x Object with argument from function call.
#'
#' @returns Simplified argument object.
#' @keywords internal
#'
#' @noRd
simplify_argument <- function(x) {
  if (is.atomic(x) || is.character(x)) {
    return(x)
  }

  if (is.name(x)) {
    return(as.character(x))
  }

  if (is.call(x)) {
    return(paste(deparse(x), collapse = ""))
  }

  x
}

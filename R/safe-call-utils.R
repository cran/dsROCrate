#' @export
as.data.frame.safe_call <- function(x, ...) {
  data.frame(
    package = x$package,
    fx = x$fx,
    argument = names(x$args),
    value = vapply(x$args, toString, character(1)),
    stringsAsFactors = FALSE
  )
}

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

enrich_call <- function(call, registry) {
  # call$args <- lapply(call$args, resolve_argument, registry = registry)
  call$args <- purrr::map(
    call$args,
    enrich_argument,
    registry = registry,
    timestamp = call$created_at,
    session = call$session
  )

  call
}

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

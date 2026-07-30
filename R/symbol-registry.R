#' @export
as.data.frame.symbol_registry <- function(x, ...) {
  as.data.frame(x$symbols)
}

#' Create new `symbol_registry` object
#'
#' @param symbols List of symbols.
#'
#' @returns New `symbol_registry` object.
#' @keywords internal
#'
#' @noRd
new_symbol_registry <- function(symbols = list()) {
  structure(list(symbols = symbols), class = "symbol_registry")
}

#' Look-up symbol in registry.
#'
#' @param symbol Symbol object.
#' @param registry Symbol registry object.
#' @param session Unique session ID.
#'
#' @returns Tibble slice with details associated to `symbol`.
#' @keywords internal
#'
#' @noRd
lookup_symbol <- function(symbol, registry, session = NULL) {
  out <- registry$symbols

  if (!is.null(session)) {
    out <- dplyr::filter(out, session == !!session)
  }

  out |>
    dplyr::filter(symbol == !!symbol) |>
    dplyr::arrange(dplyr::desc(version)) |>
    dplyr::slice(1)
}

#' Register `symbol` in `registry`
#'
#' @param registry Symbol registry object.
#' @param symbol Symbol object.
#'
#' @returns Updated symbol registry.
#' @keywords internal
#'
#' @noRd
register_symbol <- function(registry, symbol) {
  # local bindings
  aux <- session <- NULL
  stopifnot(inherits(symbol, "safe_symbol"))

  # extract current version of symbol
  if (length(registry$symbols) == 0 || nrow(registry$symbols) == 0) {
    current_version <- 0
  } else {
    aux <- registry$symbols |>
      dplyr::filter(
        session == !!symbol$session,
        symbol == !!symbol$symbol
      )
    if (nrow(aux)) {
      current_version <- aux |>
        dplyr::summarise(
          version = dplyr::coalesce(max(version), 0L)
        ) |>
        dplyr::pull(version)
    } else {
      current_version <- 0
    }
  }

  symbol$version <- current_version + 1L

  symbol$depends_on <- list(resolve_dependencies(symbol$expr, registry))

  registry$symbols <- dplyr::bind_rows(
    registry$symbols,
    tibble::as_tibble(symbol)
  )

  registry
}

#' Resolve symbol
#'
#' @param registry Symbol registry object.
#' @param symbol Symbol object.
#' @param timestamp Timestamp, when the symbol was created.
#' @param session Unique session ID.
#' @param user Username.
#'
#' @returns Tibble slice with details associated to `symbol`.
#' @keywords internal
#'
#' @noRd
resolve_symbol <- function(
  registry,
  symbol,
  timestamp,
  session = NULL,
  user = NULL
) {
  # local binding
  created_at <- NULL

  x <- registry$symbols |>
    dplyr::filter(symbol == !!symbol, created_at <= !!timestamp)

  if (!is.null(session)) {
    x <- dplyr::filter(x, session == !!session)
  }

  if (!is.null(user)) {
    x <- dplyr::filter(x, user == !!user)
  }

  if (nrow(x) == 0) {
    return(NULL)
  }

  x |>
    dplyr::slice_max(created_at, n = 1)
}

#' Create new symbol registry
#'
#' @returns New `symbol_registry` object.
#' @keywords internal
#'
#' @noRd
symbol_registry <- function() {
  new_symbol_registry()
}

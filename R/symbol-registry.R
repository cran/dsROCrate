#' @export
as.data.frame.symbol_registry <- function(x, ...) {
  do.call(
    rbind,
    lapply(
      x$symbols,
      function(sym) {
        data.frame(
          symbol = sym$symbol,
          kind = sym$kind,
          asset = sym$asset,
          parent = sym$parent,
          column = sym$column,
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

has_symbol <- function(registry, name) {
  UseMethod("has_symbol")
}

#' @export
has_symbol.symbol_registry <- function(registry, name) {
  name %in% names(registry$symbols)
}

new_symbol_registry <- function(symbols = list()) {
  structure(
    list(
      symbols = symbols
    ),
    class = "symbol_registry"
  )
}

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

register_symbol <- function(registry, symbol) {
  # local bindings
  aux <- session <- NULL
  stopifnot(inherits(symbol, "safe_symbol"))

  # extract current version of symbol
  if (nrow(registry$symbols) == 0 || length(registry$symbols) == 0) {
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

symbol_registry <- function() {
  new_symbol_registry()
}

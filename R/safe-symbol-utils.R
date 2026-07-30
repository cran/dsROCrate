#' Find symbols in a given expression
#'
#' @param expr Object with R expression.
#'
#' @returns A list with a reference, if any is found.
#' @keywords internal
#'
#' @noRd
find_symbols <- function(expr) {
  # parse expression if given value is a character
  if (is.character(expr)) {
    expr <- parse(text = expr)[[1]]
  }

  recurse <- function(x) {
    # check if the given object is a symbol
    if (is.symbol(x)) {
      return(list(list(
        symbol = as.character(x),
        column = NA_character_
      )))
    }

    # check if the given objects is NOT a call
    if (!is.call(x)) {
      return(list())
    }

    # check if the given object is of the format `symbol$column`
    if (identical(x[[1]], quote(`$`))) {
      lhs <- x[[2]]
      out <- list(list(
        symbol = as.character(lhs),
        column = as.character(x[[3]])
      ))

      return(out)
    }

    # return recursive call of the original subset
    unlist(
      lapply(as.list(x)[-1], recurse),
      recursive = FALSE
    )
  }

  refs <- recurse(expr)
  # filter out symbols from base packages
  refs <- Filter(\(x) !(x$symbol %in% c("base", "stats", "utils")), refs)
  if (!length(refs)) {
    return(NULL)
  }
  refs
}

#' Resolve expression dependencies
#'
#' @param expr Object with R expression.
#' @param registry Symbol registry object.
#'
#' @returns Tibble object with expression dependencies' details.
#' @keywords internal
#'
#' @noRd
resolve_dependencies <- function(expr, registry) {
  if (is.null(expr) || is.na(expr)) {
    return(tibble::tibble())
  }

  # find symbols for the given expression
  refs <- find_symbols(expr)

  if (is.null(refs)) {
    return(tibble::tibble())
  }

  # look up symbol details for each reference found previously
  purrr::map_dfr(refs, function(ref) {
    sym <- lookup_symbol(ref$symbol, registry)

    tibble::tibble(
      symbol_id = if (nrow(sym)) sym$id else NA_character_,
      symbol = ref$symbol,
      column = ref$column,
      kind = if (nrow(sym)) sym$kind else NA_character_,
      asset = if (nrow(sym)) sym$asset else NA_character_
    )
  })
}

#' Resolve provenance of symbol
#'
#' @param symbol_id String with unique symbol ID.
#' @param registry Symbol registry object.
#' @param visited Vector with symbol IDs that have been processed.
#'
#' @returns Tibble object with details of provenance for symbol.
#' @keywords internal
#'
#' @noRd
resolve_provenance <- function(symbol_id, registry, visited = character()) {
  # local bindings
  id <- NULL

  if (symbol_id %in% visited) {
    return(tibble::tibble())
  }

  sym <- registry$symbols |>
    dplyr::filter(id == symbol_id)

  deps <- sym$depends_on[[1]]

  if (!nrow(deps)) {
    return(tibble::tibble())
  }

  children <-
    purrr::map_dfr(
      deps$symbol_id,
      resolve_provenance,
      registry = registry,
      visited = c(visited, symbol_id)
    )

  dplyr::bind_rows(deps, children)
}

#' Resolve symbol's asset
#'
#' @param symbol_id String with unique symbol ID.
#' @param registry Symbol registry object.
#'
#' @returns String with asset(s) linked to symbol ID.
#' @keywords internal
#'
#' @noRd
resolve_symbol_asset <- function(symbol_id, registry) {
  # local binding
  asset <- id <- kind <- NULL

  sym <- registry$symbols |>
    dplyr::filter(id == !!symbol_id)

  if (!nrow(sym)) {
    return(NA_character_)
  }

  # direct asset
  if (sym$kind %in% c("table", "resource")) {
    return(sym$asset)
  }

  # expression: follow dependencies
  deps <- resolve_provenance(
    symbol_id,
    registry
  )

  if (!nrow(deps)) {
    return(NA_character_)
  }

  assets <- deps |>
    dplyr::filter(kind %in% c("table", "resource")) |>
    dplyr::pull(asset)

  if (!length(assets)) {
    return(NA_character_)
  }

  paste(unique(stats::na.omit(assets)), collapse = ";")
}

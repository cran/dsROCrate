find_symbols <- function(expr) {
  if (is.character(expr)) {
    expr <- parse(text = expr)[[1]]
  }

  recurse <- function(x) {
    if (is.symbol(x)) {
      return(list(list(
        symbol = as.character(x),
        column = NA_character_
      )))
    }

    if (!is.call(x)) {
      return(list())
    }

    if (identical(x[[1]], quote(`$`))) {
      lhs <- x[[2]]
      out <- list(list(
        symbol = as.character(lhs),
        column = as.character(x[[3]])
      ))

      return(out)
    }

    unlist(
      lapply(as.list(x)[-1], recurse),
      recursive = FALSE
    )
  }

  refs <- recurse(expr)
  refs <- Filter(\(x) !(x$symbol %in% c("base", "stats", "utils")), refs)
  if (!length(refs)) {
    return(NULL)
  }
  refs
}

resolve_dependencies <- function(expr, registry) {
  if (is.null(expr) || is.na(expr)) {
    return(tibble::tibble())
  }

  refs <- find_symbols(expr)

  if (is.null(refs)) {
    return(tibble::tibble())
  }

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

as_tibble.safe_symbol <- function(x, ...) {
  tibble::tibble(
    id = x$id,
    symbol = x$symbol,
    version = x$version,
    kind = x$kind,
    asset = x$asset,
    expr = x$expr,
    depends_on = x$depends_on,
    created_by = x$created_by,
    created_at = x$created_at,
    user = x$user,
    session = x$session,
    action = x$action
  )
}

#' Create new `safe_symbol` object
#'
#' @param symbol Symbol object.
#' @param version Version number.
#' @param kind Type of symbol.
#' @param asset Associated asset.
#' @param expr Associated expression.
#' @param depends_on Symbol dependencies.
#' @param created_by DataSHIELD function used to create the symbol.
#' @param created_at Creation date.
#' @param user Username.
#' @param session Unique session ID.
#' @param action Action type.
#' @param metadata Additional metadata.
#' @param id Unique symbol ID.
#'
#' @returns New `safe_symbol` object.
#' @keywords internal
#'
#' @noRd
new_safe_symbol <- function(
  symbol,
  version = -999,
  kind = "unknown",
  asset = NA_character_,
  expr = NA_character_,
  depends_on = list(),
  created_by = NA_character_,
  created_at = Sys.time(),
  user = NA_character_,
  session = NA_character_,
  action = NA_character_,
  metadata = list(),
  id = paste0("symbol-", uuid::UUIDgenerate())
) {
  stopifnot(is.character(symbol))
  stopifnot(length(symbol) == 1)

  structure(
    list(
      id = id,
      symbol = symbol,
      version = version,
      kind = kind,
      asset = asset,
      expr = expr,
      depends_on = depends_on,
      created_by = created_by,
      created_at = created_at,
      user = user,
      session = session,
      action = action,
      metadata = metadata
    ),
    class = "safe_symbol"
  )
}

#' Safe symbol details
#'
#' @param symbol Symbol object.
#' @param ... Additional arguments.
#'
#' @returns Object with the class `safe_symbol`.
#' @keywords internal
#'
#' @noRd
safe_symbol <- function(symbol, ...) {
  new_safe_symbol(symbol = symbol, ...)
}

#' Safe reference details
#'
#' @param symbol Symbol object.
#' @param symbol_id String with symbol unique ID.
#' @param column String with column name, when symbol is `symbol$column`.
#'
#' @returns Object with the class `safe_reference`
#' @keywords internal
#'
#' @noRd
safe_reference <- function(symbol, symbol_id, column = NULL) {
  structure(
    list(
      symbol = symbol,
      symbol_id = symbol_id,
      column = column
    ),
    class = "safe_reference"
  )
}

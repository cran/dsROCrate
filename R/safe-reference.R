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

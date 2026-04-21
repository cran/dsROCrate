#' Unfill vector
#'
#' @param x Input vector.
#' @param val Default value.
#'
#' @returns Unfilled vector.
#' @keywords internal
#' @source https://github.com/tidyverse/tidyr/issues/250#issuecomment-344984802
unfill_vec <- function(x, val = "") {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, val, x)
}

#' Fill vector
#'
#' @param x Input vector.
#' @param val Default value.
#'
#' @returns Filled vector.
#' @keywords internal
refill_vec <- function(x, val = "") {
  for (i in seq_along(x)) {
    if (i > 1 && (is.na(x[i]) || x[i] == val)) {
      x[i] <- x[i - 1]
    }
  }
  x
}

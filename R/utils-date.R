#' Validate POSIXct string
#'
#' @param x POSIXct string.
#' @param tz String with time zone,
#'
#' @returns Boolean value to indicate if the given string is a valid POSIXct
#' string.
#' @keywords internal
is_valid_posixct <- function(x, tz = "UTC") {
  tryCatch(
    {
      !is.na(as.POSIXct(
        x,
        tz = tz,
        tryFormats = c(
          "%Y-%m-%d %H:%M:%S",
          "%Y/%m/%d %H:%M:%S"
        )
      ))
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

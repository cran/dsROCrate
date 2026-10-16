fmt_limit <- function(x) {
  if (is.infinite(x)) "ALL" else format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

format_utc <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    attr(x, "tzone") <- "UTC" # same instant, only the display changes
    return(x)
  }
  x <- trimws(as.character(x))
  x <- sub("^(\\d{4}-\\d{2}-\\d{2}) ", "\\1T", x) # allow a space separator

  # split "YYYY-mm-ddTHH:MM:SS.XX+YY:ZZ" into date-time and offset
  offset_pattern <- "(Z|[+-][0-9]{2}:?[0-9]{2})$"
  has_offset <- grepl(offset_pattern, x)
  offset <- ifelse(has_offset, sub(paste0(".*", offset_pattern), "\\1", x), "Z")
  datetime <- sub(offset_pattern, "", x)

  # offset in seconds (no offset or "Z" means UTC)
  digits <- gsub("[^0-9]", "", offset)
  offset_secs <- ifelse(
    offset == "Z",
    0,
    ifelse(startsWith(offset, "-"), -1, 1) *
      (as.numeric(substr(digits, 1, 2)) *
        3600 +
        as.numeric(substr(digits, 3, 4)) * 60)
  )

  as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") - offset_secs
}

parse_utc_limit <- function(x) {
  if (is.numeric(x) && length(x) == 1 && is.infinite(x)) x else parse_utc(x)
}

#' @noRd
id_hash <- function(prefix, x) {
  paste0(prefix, digest::digest(x))
}

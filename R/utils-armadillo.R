#' Login to a MOLGENIS' Armadillo server
#'
#' Login to a MOLGENIS' Armadillo server. Wrapper for the function
#' [DSMolgenisArmadillo::armadillo.get_credentials()].
#'
#' @inheritParams DSMolgenisArmadillo::armadillo.get_credentials
#'
#' @returns MOLGENIS' Armadillo connection object.
#' @export
armadillo_login <- function(server) {
  conn <- DSMolgenisArmadillo::armadillo.get_credentials(server)
  ns <- loadNamespace("MolgenisArmadillo")
  pkgenv <- get(".pkgglobalenv", envir = ns)
  assign("armadillo_url", server, envir = pkgenv)
  assign("auth_token", getElement(conn, "access_token"), envir = pkgenv)
  return(invisible(conn))
}

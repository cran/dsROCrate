# -------------------------------------------------------------------------
# Helper: safely get MolgenisArmadillo .pkgglobalenv (if package available)
# -------------------------------------------------------------------------
get_pkgenv <- function() {
  ns <- loadNamespace("MolgenisArmadillo")
  get(".pkgglobalenv", envir = ns)
}

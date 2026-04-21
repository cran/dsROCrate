#' Open connection to OBiBa's Opal demo server
#'
#' @param admin Boolean flag to indicate if the connection should be with admin
#'     rights (default behaviour).
#'
#' @returns Connection object to OBiBa's Opal demo server with extra attributes.
opal_demo_con <- function(admin = TRUE) {
  # Skip tests if offline or on CRAN
  skip_on_cran()
  skip_if_offline()

  ## Opal server access
  USERNAME <- "administrator"
  USERPASS <- "password"
  SERVER <- "https://opal-demo.obiba.org"
  ## Credentials for `dsuser`
  ### NOTE: this is only used to simulate an analysis and generate logs
  DSUSERPASS <- "P@ssw0rd"

  ## Five safes variables
  PEOPLE <- "dsuser"
  PROJECT <- "CNSIM"
  TABLES <- c("CNSIM1")

  # login to local server with `USERNAME` and `USERPASS`.
  ## as administrator
  if (admin) {
    opal_con <- opalr::opal.login(
      username = USERNAME,
      password = USERPASS,
      url = SERVER
    )
  } else {
    # as non-admin user
    opal_con <- opalr::opal.login(
      username = PEOPLE,
      password = DSUSERPASS,
      url = SERVER
    )
  }

  attr(opal_con, "USERNAME") <- USERNAME
  attr(opal_con, "USERPASS") <- USERPASS
  attr(opal_con, "SERVER") <- SERVER
  attr(opal_con, "DSUSERPASS") <- DSUSERPASS
  attr(opal_con, "PEOPLE") <- PEOPLE
  attr(opal_con, "PROJECT") <- PROJECT
  attr(opal_con, "TABLES") <- TABLES

  return(opal_con)
}

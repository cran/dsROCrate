## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

SERVER <- "https://opal-demo.obiba.org"

# The examples need OBiBa's public Opal demo server. If it cannot be reached
# (e.g., when building offline), the code is shown but not run.
demo_available <- local({
  old <- options(timeout = 5)
  on.exit(options(old))
  tryCatch(
    {
      con <- url(SERVER, open = "rb")
      close(con)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
})
knitr::opts_chunk$set(eval = demo_available)

## ----setup, eval = TRUE-------------------------------------------------------
library(dsROCrate)

## ----connect------------------------------------------------------------------
USERNAME <- "administrator"
USERPASS <- "password"

con <- opalr::opal.login(
  username = USERNAME,
  password = USERPASS,
  url = SERVER
)

print(con)

## ----check_permissions--------------------------------------------------------
check_permissions(con, verbose = TRUE)

## ----audit, warning = FALSE---------------------------------------------------
PROJECT <- "CNSIM"
TABLES <- "CNSIM1"
USER <- "dsuser"

crate <- audit(con, project = PROJECT, tables = TABLES, user = USER)

print(crate)

## ----report_overview----------------------------------------------------------
report_file <- tempfile(fileext = ".Rmd")

report_parts <- crate |>
  report(filepath = report_file, render = FALSE)

# overview diagram
report_parts$overview_diagram

# overview table (Safe People, Safe Project and Safe Data)
knitr::kable(report_parts$overview_data)

## ----report_render, eval = FALSE----------------------------------------------
# crate |>
#   report(
#     filepath = report_file,
#     title = "DataSHIELD Five Safes report",
#     render = TRUE,
#     overwrite = TRUE
#   )

## ----logout-------------------------------------------------------------------
opalr::opal.logout(con)

## ----cleanup, include = FALSE-------------------------------------------------
unlink(report_file, force = TRUE)


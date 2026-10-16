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

cons <- list(
  site_a = opalr::opal.login(USERNAME, USERPASS, url = SERVER),
  site_b = opalr::opal.login(USERNAME, USERPASS, url = SERVER)
)

## ----audit, warning = FALSE---------------------------------------------------
study_audit <- cons |>
  audit(project = "CNSIM", tables = "CNSIM1", user = "dsuser")

# one RO-Crate per site
names(study_audit)

print(study_audit$site_a)

## ----report_overview----------------------------------------------------------
report_file <- tempfile(fileext = ".Rmd")

study_parts <- study_audit |>
  report(filepath = report_file, render = FALSE)

# overview diagram
study_parts$overview_diagram

# overview table (Safe People, Safe Project and Safe Data)
knitr::kable(study_parts$overview_data)

## ----report_render, eval = FALSE----------------------------------------------
# study_audit |>
#   report(
#     filepath = report_file,
#     title = "DataSHIELD study audit",
#     render = TRUE,
#     overwrite = TRUE
#   )

## ----logout-------------------------------------------------------------------
invisible(lapply(cons, opalr::opal.logout))

## ----cleanup, include = FALSE-------------------------------------------------
unlink(report_file, force = TRUE)


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

# the DataSHIELD client packages are only needed to generate some activity
ds_clients_available <- all(
  vapply(
    c("DSI", "DSOpal", "dsBaseClient"),
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
)

# helper: count the entities in a crate by type
crate_summary <- function(crate) {
  types <- vapply(
    crate$`@graph`,
    function(e) {
      t <- e[["@type"]]
      if (is.null(t)) NA_character_ else t[1]
    },
    character(1)
  )
  knitr::kable(as.data.frame(table(Type = types, useNA = "ifany"),
                             responseName = "Entities"))
}

options(max_lines = 40)

## ----setup, eval = TRUE-------------------------------------------------------
library(dsROCrate)

## ----connect------------------------------------------------------------------
# server access (administrator account on the demo server)
USERNAME <- "administrator"
USERPASS <- "password"
# credentials of `dsuser`, only used to generate activity in section 3
DSUSERPASS <- "P@ssw0rd"

# Five Safes details
PROJECT <- "CNSIM"
TABLES <- "CNSIM1"
USER <- "dsuser"

con <- opalr::opal.login(
  username = USERNAME,
  password = USERPASS,
  url = SERVER
)

## ----init---------------------------------------------------------------------
crate <- con |>
  init(project = PROJECT, tables = TABLES, user = USER)

crate_summary(crate)

## ----activity, eval = demo_available && ds_clients_available, results = "hide"----
# registers the OpalDriver class in the current session
DSOpal::Opal()

builder <- DSI::newDSLoginBuilder()
builder$append(
  server = "study1",
  url = SERVER,
  user = USER,
  password = DSUSERPASS,
  driver = "OpalDriver"
)
conns <- DSI::datashield.login(logins = builder$build())

DSI::datashield.assign.table(
  conns["study1"],
  symbol = "dsROCrate_test",
  table = paste0(PROJECT, ".", TABLES[1]),
  errors.print = TRUE
)
dsBaseClient::ds.ls(datasources = conns["study1"])
dsBaseClient::ds.summary("dsROCrate_test", datasources = conns["study1"])

DSI::datashield.logout(conns)

## ----safe_people--------------------------------------------------------------
crate <- crate |> safe_people()

crate_summary(crate)

## ----safe_project-------------------------------------------------------------
crate <- crate |> safe_project()

crate_summary(crate)

## ----safe_data----------------------------------------------------------------
crate <- crate |> safe_data()

crate_summary(crate)

## ----safe_setting-------------------------------------------------------------
crate <- crate |> safe_setting()

crate_summary(crate)

## ----safe_output--------------------------------------------------------------
crate <- crate |>
  safe_output(
    logs_from = Sys.time() - 24 * 60^2,
    logs_to = Sys.time()
  )

crate_summary(crate)

## ----bag----------------------------------------------------------------------
# temporary directory, only for demo purposes
tmp_path_bag <- file.path(tempdir(), "dsROCrate-building-crates")
dir.create(tmp_path_bag, showWarnings = FALSE)

path_to_rocrate_bag <- crate |>
  rocrateR::bag_rocrate(path = tmp_path_bag, overwrite = TRUE)

## ----unbag, eval = demo_available && requireNamespace("fs", quietly = TRUE)----
path_to_rocrate_bag |>
  rocrateR::unbag_rocrate(
    output = file.path(tmp_path_bag, "ROC"),
    quiet = TRUE
  ) |>
  fs::dir_tree()

## ----logout-------------------------------------------------------------------
opalr::opal.logout(con)
unlink(tmp_path_bag, recursive = TRUE, force = TRUE)

## ----reset, include = FALSE---------------------------------------------------
options(max_lines = NULL)


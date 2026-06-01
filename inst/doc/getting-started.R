## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE,
  out.width = "100%",
  cache = FALSE,
  screenshot.opts = list(vwidth = 2000, vheight = 600, zoom = 3, selector = "div.html-widget")
)

# save the built-in output hook
hook_output <- knitr::knit_hooks$get("output")
hook_message <- knitr::knit_hooks$get("message")

# define a truncation helper
truncate_lines_tail <- function(x, lines) {
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines) == 1) {
    if (length(x) > lines) {
      # truncate the output, but add `more`
      # x <- c(head(x, n = lines), more)
      x <- c(more, tail(x, n = lines))
    }
  } else {
    x <- c(more, x[lines], more)
  }
  paste(c(x, ""), collapse = "\n")
}

# set the new hooks
knitr::knit_hooks$set(
  output = function(x, options) {
    lines <- options$out.lines
    if (is.null(lines)) return(hook_output(x, options))
    x <- truncate_lines_tail(x, lines)
    return(hook_output(x, options))
  },
  message = function(x, options) {
    lines <- options$out.lines
    if (is.null(lines)) return(hook_message(x, options))
    x <- truncate_lines_tail(x, lines)
    return(hook_message(x, options))
  }
)

# helper function to get the number of rows of an RO-Crate
rocrate_lines <- function(rocrate) {
  rocrate_txt(rocrate) |>
    length()
}

# helper function to read RO-Crate as text file
rocrate_txt <- function(rocrate) {
  # create tmp file to write a JSON file with the output
  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file, force = TRUE))
  # write RO-Crate in tmp file
  rocrateR::write_rocrate(rocrate, tmp_file)
  # read RO-Crate as text file
  readLines(tmp_file)
}

## ----setup--------------------------------------------------------------------
library(dsROCrate)

# show all the lines in the RO-Crate
oopt <- options(max_lines = Inf)

## ----eval = FALSE-------------------------------------------------------------
# vignette("deploy-local-datashield-server-with-opal", package = "dsROCrate")

## -----------------------------------------------------------------------------
# define global variables
## Opal server access
USERNAME <- "administrator"
USERPASS <- "password"
SERVER <- "https://opal-demo.obiba.org"
## Credentials for `dsuser`
### NOTE: this is only used to simulate an analysis and generate logs
DSUSERPASS <- "P@ssw0rd"

## -----------------------------------------------------------------------------
## Five safes variables
PEOPLE <- "dsuser"
PROJECT <- "CNSIM"
TABLES <- c("CNSIM1")

## -----------------------------------------------------------------------------
# login to local server with `USERNAME` and `USERPASS`.
o <- opalr::opal.login(
  username = USERNAME,
  password = USERPASS,
  url = SERVER
)

print(o)

## ----eval = FALSE, echo = TRUE------------------------------------------------
# # install.packages("pak")
# pak::pak("rocrateR")
# 
# # for development version use
# pak::pak("ResearchObject/ro-crate-r@dev")

## -----------------------------------------------------------------------------
basic_rocrate <- rocrateR::rocrate_5s()

## -----------------------------------------------------------------------------
print(basic_rocrate)

## ----out.lines=-(rocrate_lines(basic_rocrate) - 2)----------------------------
basic_rocrate <- o |>
  dsROCrate::safe_data(rocrate = basic_rocrate,
                       project = PROJECT,
                       tables = TABLES)

print(basic_rocrate) # note that the output will be truncated

## ----out.lines=-(rocrate_lines(basic_rocrate) - 2)----------------------------
basic_rocrate <- o |>
  dsROCrate::safe_project(rocrate = basic_rocrate,
                          project = PROJECT)

print(basic_rocrate) # note that the output will be truncated

## ----safe_people, out.lines=-(rocrate_lines(basic_rocrate) + 1)---------------
basic_rocrate <- o |>
  dsROCrate::safe_people(rocrate = basic_rocrate, user = PEOPLE)

print(basic_rocrate) # note that the output will be truncated

## ----eval = FALSE-------------------------------------------------------------
# # close previous connection
# opalr::opal.logout(o)
# 
# # open new connection as administrator
# o <- opalr::opal.login(
#   username = "administrator",
#   password = "password",
#   url = SERVER
# )

## ----out.lines=-(rocrate_lines(basic_rocrate) - 2)----------------------------
basic_rocrate <- o |>
  dsROCrate::safe_setting(rocrate = basic_rocrate)

print(basic_rocrate) # note that the output will be truncated

## ----eval = FALSE-------------------------------------------------------------
# # close previous connection
# opalr::opal.logout(o)
# 
# # open new connection as administrator
# o <- opalr::opal.login(
#   username = "administrator",
#   password = "password",
#   url = SERVER
# )

## ----eval = FALSE-------------------------------------------------------------
# pak::pak("DSI")
# pak::pak("DSOpal")
# pak::pak("dsBaseClient")

## ----eval = TRUE--------------------------------------------------------------
# run some test commands with dsBaseClient
## needed to defined the OpalDriver class in the current environment
DSOpal::Opal()
## create new login object, note that here we use the `dsuser`
builder <- DSI::newDSLoginBuilder()
builder$append(server = "study1",
               url = SERVER,
               user = "dsuser",
               password = DSUSERPASS,
               driver = "OpalDriver")
logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata)

## ----eval = FALSE-------------------------------------------------------------
# ## assign data
# DSI::datashield.assign.table(conns["study1"],
#                              symbol = "dsROCrate_test",
#                              table = paste0(PROJECT, ".", TABLES[1]),
#                              errors.print = TRUE)
# 
# dsBaseClient::ds.ls(datasources = conns["study1"])
# #> $study1
# #> $study1$environment.searched
# #> [1] "R_GlobalEnv"
# #>
# #> $study1$objects.found
# #> [1] "dsROCrate_test"
# dsBaseClient::ds.summary("dsROCrate_test")
# #> $study1
# #> $study1$class
# #> [1] "data.frame"
# #>
# #> $study1$`number of rows`
# #> [1] 2163
# #>
# #> $study1$`number of columns`
# #> [1] 11
# #>
# #> $study1$`variables held`
# #>  [1] "LAB_TSC"            "LAB_TRIG"           "LAB_HDL"
# #>  [4] "LAB_GLUC_ADJUSTED"  "PM_BMI_CONTINUOUS"  "DIS_CVA"
# #>  [7] "MEDI_LPD"           "DIS_DIAB"           "DIS_AMI"
# #> [10] "GENDER"             "PM_BMI_CATEGORICAL"

## ----echo = FALSE, warning = FALSE, message = FALSE, results='hide'-----------
# check if there are any logs available, if not simulate some operations
dsuser_logs_tbl <- opalr::opal.login(
    username = "administrator",
    password = "password",
    url = SERVER
) |> 
  opalr::dsadmin.log() |>
  dplyr::bind_rows(tibble::tibble(username = NA, ds_action = NA)) |>
  dplyr::filter(!is.na(username), username == "dsuser") |>
  dplyr::filter(ds_action %in% c("ASSIGN", "AGGREGATE", "OPEN"))

if (nrow(dsuser_logs_tbl) < 1) {
  ## assign data
  DSI::datashield.assign.table(conns["study1"], 
                               symbol = "dsROCrate_test",
                               table = paste0(PROJECT, ".", TABLES[1]),
                               errors.print = TRUE)
  dsBaseClient::ds.ls(datasources = conns["study1"])
  dsBaseClient::ds.summary("dsROCrate_test")
}

## ----safe_outputs_internal, echo=FALSE----------------------------------------
lines_before_safe_outputs <- rocrate_lines(basic_rocrate)

## ----safe_outputs-------------------------------------------------------------
basic_rocrate <- o |>
  dsROCrate::safe_output(rocrate = basic_rocrate,
                         logs_from = Sys.time() - 8.64E4, # capture the last 24 hours
                         logs_to = Sys.time())

## ----out.lines=-(lines_before_safe_outputs + 6)-------------------------------
print(basic_rocrate) # note that the output will be truncated

## -----------------------------------------------------------------------------
opalr::opal.logout(o)

## -----------------------------------------------------------------------------
# create temp directory (only for demo purposes)
tmp_path_bag <- file.path(tempdir(), "dsROCrate-getting-started")
dir.create(tmp_path_bag, showWarnings = FALSE)

# create RO-Crate bag
path_to_rocrate_bag <- basic_rocrate |>
  rocrateR::bag_rocrate(path = tmp_path_bag, overwrite = TRUE)

## -----------------------------------------------------------------------------
# extract files in temporary directory
path_to_rocrate_bag |>
  # extract contents inside /tmp_path_bag/ROC
  rocrateR::unbag_rocrate(output = file.path(tmp_path_bag, "ROC"), quiet = TRUE) |>
  # create tree with the files
  fs::dir_tree()

## -----------------------------------------------------------------------------
unlink(tmp_path_bag, recursive = TRUE, force = TRUE)

## ----echo = FALSE-------------------------------------------------------------
options(oopt)
oopt <- options(max_lines = 40)

## ----warning=FALSE------------------------------------------------------------
safe_people_crate_v1 <- opalr::opal.login(
  username = USERNAME,
  password = USERPASS,
  url = SERVER
) |>
  dsROCrate::audit(user = "dsuser", project = "CNSIM")

print(safe_people_crate_v1)

## ----safe_people_crate_audit_v1-----------------------------------------------
safe_people_crate_v1_rmd <- tempfile(fileext = ".Rmd") # temporary file

safe_people_crate_contents <- safe_people_crate_v1 |>
  dsROCrate::report(filepath = safe_people_crate_v1_rmd, render = FALSE)

# display Overview diagram
safe_people_crate_contents$overview_diagram

# display Overview data (Safe People, Safe Projects and Safe Data)
safe_people_crate_contents$overview_data |>
  knitr::kable()

## ----eval = FALSE-------------------------------------------------------------
# safe_people_crate_v1 |>
#   dsROCrate::report(filepath = safe_people_crate_v1_rmd,
#                             title = "DataSHIELD Safe People - Audit Report",
#                             render = TRUE,
#                             overwrite = TRUE)

## ----warning=FALSE------------------------------------------------------------
safe_project_crate_v1 <- opalr::opal.login(
  username = USERNAME,
  password = USERPASS,
  url = SERVER
) |>
  dsROCrate::audit(project = "CNSIM")

print(safe_project_crate_v1)

## ----safe_project_crate_audit_v1----------------------------------------------
safe_project_crate_v1_rmd <- tempfile(fileext = ".Rmd") # temporary file

safe_project_crate_contents <- safe_project_crate_v1 |>
  dsROCrate::report(filepath = safe_project_crate_v1_rmd, render = FALSE)

# display Overview diagram
safe_project_crate_contents$overview_diagram

# display Overview data (Safe People, Safe Projects and Safe Data)
safe_project_crate_contents$overview_data |>
  knitr::kable()

## ----eval = FALSE-------------------------------------------------------------
# safe_project_crate_v1 |>
#   dsROCrate::report(filepath = safe_project_crate_v1_rmd,
#                             title = "DataSHIELD Safe Project - Audit Report",
#                             render = TRUE,
#                             overwrite = TRUE)

## ----warning=FALSE------------------------------------------------------------
study_crate_v1 <- 
  list(
    "opal_test" = opalr::opal.login(
      username = USERNAME,
      password = USERPASS,
      url = "https://opal-demo.obiba.org"
    ),
    "opal_demo" = opalr::opal.login(
      username = USERNAME,
      password = USERPASS,
      url = "https://opal-demo.obiba.org"
    )
  ) |>
  dsROCrate::audit(project = "CNSIM")

print(study_crate_v1)

## ----study_crate_audit_v1-----------------------------------------------------
study_crate_v1_rmd <- tempfile(fileext = ".Rmd") # temporary file

safe_project_crate_contents <- study_crate_v1 |>
  dsROCrate::report(filepath = study_crate_v1_rmd, render = FALSE)

# display Overview diagram
safe_project_crate_contents$overview_diagram

# display Overview data (Safe People, Safe Projects and Safe Data)
safe_project_crate_contents$overview_data |>
  knitr::kable()

## ----eval = FALSE-------------------------------------------------------------
# study_crate_v1 |>
#   dsROCrate::report(filepath = study_crate_v1_rmd,
#                             title = "DataSHIELD Study audit",
#                             render = TRUE,
#                             overwrite = TRUE)

## ----echo = FALSE, message = FALSE, error = FALSE-----------------------------
unlink(safe_people_crate_v1_rmd, TRUE, TRUE)
unlink(safe_project_crate_v1_rmd, TRUE, TRUE)
unlink(study_crate_v1_rmd, TRUE, TRUE)
# reverse options
options(oopt)


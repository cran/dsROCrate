# test_that("project_exists.opal() succeeds when project exists", {
#   # setup
#   opal_con <- opal_demo_con()
#
#   expect_silent(
#     dsROCrate:::project_exists(opal_con, project = attr(opal_con, "PROJECT"))
#   )
# })
#
# test_that("project_exists.opal() errors when project does not exist", {
#   expect_error(
#     project_exists(opal_con, project = "THIS_PROJECT_DOES_NOT_EXIST"),
#     "was not found in the given Opal connection!"
#   )
# })
#
# test_that("project_exists() dispatches to opal method", {
#   expect_silent(
#     project_exists(opal_con, project = attr(opal_con, "PROJECT"))
#   )
# })
#
# test_that("project_exists() works for armadillo objects", {
#   skip_if_not_installed("MolgenisArmadillo")
#
#   arm <- methods::new("armadillo")
#
#   local_mocked_bindings(
#     armadillo.list_projects = function() c("proj1", "proj2"),
#     .package = "MolgenisArmadillo"
#   )
#
#   expect_silent(
#     project_exists(arm, project = "proj1")
#   )
#
#   expect_error(
#     project_exists(arm, project = "missing"),
#     "was not found in the given Armadillo connection!"
#   )
# })

#' Build a minimal fake `opal`-classed object for offline/mocked tests
#'
#' Carries just enough shape (a `username` field and the `"opal"` class) for
#' S3 dispatch to resolve to the `.opal` methods. It is never passed to a
#' real network call in these tests: every function that would otherwise
#' talk to a server (`backend_*()`, `validate_backend()`, etc.) is replaced
#' first via `testthat::local_mocked_bindings()`, so no live connection - and
#' therefore no `skip_on_cran()`/`skip_if_offline()` - is required.
fake_opal_con <- function(username = "test_user") {
  structure(
    list(username = username),
    class = "opal"
  )
}

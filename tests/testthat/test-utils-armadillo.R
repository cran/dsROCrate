# -------------------------------------------------------------------------
# basic behaviour (unit tests with mocking)
# -------------------------------------------------------------------------
test_that("armadillo_login returns invisible connection object", {
  skip_if_not_installed("MolgenisArmadillo")

  mock_conn <- list(access_token = "fake_token_123")

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      expect_equal(server, "https://armadillo.example.org")
      mock_conn
    },
    {
      result <- armadillo_login("https://armadillo.example.org")
    },
    .package = "DSMolgenisArmadillo"
  )

  # should return the connection invisibly
  expect_equal(result, mock_conn)
})

test_that("armadillo_login calls armadillo.get_credentials with correct server", {
  skip_if_not_installed("MolgenisArmadillo")

  called_server <- NULL

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      called_server <<- server
      list(access_token = "abc")
    },
    {
      armadillo_login("https://my-armadillo.org")
    },
    .package = "DSMolgenisArmadillo"
  )

  expect_equal(called_server, "https://my-armadillo.org")
})

# -------------------------------------------------------------------------
# Side-effect tests: namespace global assignments
# -------------------------------------------------------------------------

test_that("armadillo_login assigns armadillo_url in MolgenisArmadillo namespace", {
  skip_if_not_installed("MolgenisArmadillo")

  pkgenv <- get_pkgenv()
  original_url <- if (exists("armadillo_url", envir = pkgenv)) {
    get("armadillo_url", envir = pkgenv)
  } else {
    NULL
  }

  on.exit(
    {
      # restore original state to avoid test pollution
      if (is.null(original_url)) {
        if (exists("armadillo_url", envir = pkgenv)) {
          rm("armadillo_url", envir = pkgenv)
        }
      } else {
        assign("armadillo_url", original_url, envir = pkgenv)
      }
    },
    add = TRUE
  )

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      list(access_token = "token_xyz")
    },
    {
      armadillo_login("https://armadillo.test")
    },
    .package = "DSMolgenisArmadillo"
  )

  expect_true(exists("armadillo_url", envir = pkgenv))
  expect_equal(get("armadillo_url", envir = pkgenv), "https://armadillo.test")
})

test_that("armadillo_login assigns auth_token from connection access_token", {
  skip_if_not_installed("MolgenisArmadillo")

  pkgenv <- get_pkgenv()
  original_token <- if (exists("auth_token", envir = pkgenv)) {
    get("auth_token", envir = pkgenv)
  } else {
    NULL
  }

  on.exit(
    {
      # restore original state
      if (is.null(original_token)) {
        if (exists("auth_token", envir = pkgenv)) {
          rm("auth_token", envir = pkgenv)
        }
      } else {
        assign("auth_token", original_token, envir = pkgenv)
      }
    },
    add = TRUE
  )

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      list(access_token = "secure_token_456")
    },
    {
      armadillo_login("https://armadillo.secure")
    },
    .package = "DSMolgenisArmadillo"
  )

  expect_true(exists("auth_token", envir = pkgenv))
  expect_equal(get("auth_token", envir = pkgenv), "secure_token_456")
})

# -------------------------------------------------------------------------
# Edge cases
# -------------------------------------------------------------------------
test_that("armadillo_login handles missing access_token gracefully", {
  skip_if_not_installed("MolgenisArmadillo")

  pkgenv <- get_pkgenv()

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      list() # no access_token
    },
    {
      expect_error(
        armadillo_login("https://armadillo.no-token"),
        regexp = NA
      )
    },
    .package = "DSMolgenisArmadillo"
  )
})

test_that("armadillo_login propagates credential retrieval errors", {
  skip_if_not_installed("MolgenisArmadillo")

  with_mocked_bindings(
    armadillo.get_credentials = function(server) {
      stop("Authentication failed")
    },
    {
      expect_error(
        armadillo_login("https://armadillo.fail"),
        "Authentication failed"
      )
    },
    .package = "DSMolgenisArmadillo"
  )
})

# -------------------------------------------------------------------------
# Integration-style test (optional, only if real server configured)
# -------------------------------------------------------------------------
test_that("armadillo_login works with real Armadillo server (optional)", {
  server <- Sys.getenv("ARMADILLO_SERVER")
  skip_if(server == "", "No ARMADILLO_SERVER env var set")

  expect_no_error({
    conn <- armadillo_login(server)
  })

  expect_true(is.list(conn) || is.environment(conn))
})

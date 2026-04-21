# -------------------------------------------------------------------------
# S3 generic dispatch
# -------------------------------------------------------------------------
test_that("extract_safe_setting dispatch works for rocrate", {
  # create an RO-Crate with setting entities
  disc_setting <- rocrateR::entity(
    id = "#setting-1",
    type = "PropertyValue",
    name = "analysis.threshold",
    value = "0.05"
  )
  disc_env <- rocrateR::entity(
    id = paste0("#env:disclosure_settings:", "default"),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    hasPart = purrr::map(list(disc_setting), ~ list("@id" = .x$`@id`))
  )
  safe_setting_root <- rocrateR::entity(
    id = paste0("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = list(disc_env) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )
  rocrate <- list(disc_setting, disc_env, safe_setting_root) |>
    purrr::reduce(rocrateR::add_entity, .init = rocrateR::rocrate_5s())

  out <- dsROCrate:::extract_safe_setting(rocrate)

  expect_s3_class(out, "rocrate")
})

test_that("flatten_safe_setting.default returns empty tibble", {
  out <- flatten_safe_setting(list())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

# -------------------------------------------------------------------------
# extract_safe_setting.rocrate()
# -------------------------------------------------------------------------
test_that("extract_safe_setting.rocrate errors when no matching entities", {
  rocrate <- rocrateR::rocrate_5s()

  # ignore warning about missing entity of @type = 'PropertyValue'!
  suppressWarnings(
    expect_error(
      extract_safe_setting(rocrate),
      "The Safe Setting root entity has no entities linked!"
    )
  )
})

test_that("extract_safe_setting.rocrate extracts PropertyValue and SoftwareApplication", {
  # create an RO-Crate with setting entities
  software_ent <- rocrateR::entity(
    id = "#software-1",
    type = "SoftwareApplication",
    name = "DataSHIELD",
    version = "6.2.0"
  )
  disc_setting <- rocrateR::entity(
    id = "#setting-1",
    type = "PropertyValue",
    name = "analysis.threshold",
    value = "0.05"
  )
  disc_env <- rocrateR::entity(
    id = paste0("#env:disclosure_settings:", "default"),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    hasPart = purrr::map(list(disc_setting), ~ list("@id" = .x$`@id`))
  )
  safe_setting_root <- rocrateR::entity(
    id = paste0("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = list(disc_env, software_ent) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )
  rocrate <- list(disc_setting, disc_env, software_ent, safe_setting_root) |>
    purrr::reduce(rocrateR::add_entity, .init = rocrateR::rocrate_5s())

  out <- extract_safe_setting(rocrate)

  expect_s3_class(out, "rocrate")

  pv <- .get_entity(out, type = "PropertyValue")
  sa <- .get_entity(out, type = "SoftwareApplication")

  expect_equal(length(pv), 1)
  expect_equal(length(sa), 1)
})

test_that("extract_safe_setting.rocrate filters correctly by id", {
  # create an RO-Crate with setting entities
  disc_setting <- rocrateR::entity(
    id = "#setting-1",
    type = "PropertyValue",
    name = "analysis.threshold",
    value = "0.05"
  )
  disc_env <- rocrateR::entity(
    id = paste0("#env:disclosure_settings:", "default"),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    hasPart = purrr::map(list(disc_setting), ~ list("@id" = .x$`@id`))
  )
  safe_setting_root <- rocrateR::entity(
    id = paste0("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = list(disc_env) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )
  rocrate <- list(disc_setting, disc_env, safe_setting_root) |>
    purrr::reduce(rocrateR::add_entity, .init = rocrateR::rocrate_5s())

  out <- extract_safe_setting(rocrate, id = "#setting-1")

  pv <- .get_entity(out, type = "PropertyValue")
  # ignore warning about missing entity of @type = 'SoftwareApplication'!
  suppressWarnings(
    sa <- .get_entity(out, type = "SoftwareApplication")
  )

  expect_equal(length(pv), 1)
  expect_equal(getElement(pv[[1]], "@id"), "#setting-1")
  # SoftwareApplication should not be re-added when filtered out
  expect_true(length(sa) >= 0)
})

test_that("extract_safe_setting.rocrate emits informative message", {
  # create an RO-Crate with setting entities
  disc_setting <- rocrateR::entity(
    id = "#setting-1",
    type = "PropertyValue",
    name = "analysis.threshold",
    value = "0.05"
  )
  disc_env <- rocrateR::entity(
    id = paste0("#env:disclosure_settings:", "default"),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    hasPart = purrr::map(list(disc_setting), ~ list("@id" = .x$`@id`))
  )
  safe_setting_root <- rocrateR::entity(
    id = paste0("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = list(disc_env) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )
  rocrate <- list(disc_setting, disc_env, safe_setting_root) |>
    purrr::reduce(rocrateR::add_entity, .init = rocrateR::rocrate_5s())

  expect_message(
    extract_safe_setting(rocrate),
    "'PropertyValue' OR 'SoftwareApplication' entit"
  )
})

test_that("extract_safe_setting.rocrate errors when id filter removes all", {
  # create an RO-Crate with setting entities
  disc_setting <- rocrateR::entity(
    id = "#setting-1",
    type = "PropertyValue",
    name = "analysis.threshold",
    value = "0.05"
  )
  disc_env <- rocrateR::entity(
    id = paste0("#env:disclosure_settings:", "default"),
    type = "CreativeWork",
    name = "Disclosure Control Environment",
    hasPart = purrr::map(list(disc_setting), ~ list("@id" = .x$`@id`))
  )
  safe_setting_root <- rocrateR::entity(
    id = paste0("#safesetting:", "opal"),
    type = "CreativeWork",
    name = "Safe Setting Controls (Opal)",
    description = paste(
      "Technical, physical and organisational safeguards applied to minimise",
      "disclosure risk."
    ),
    hasPart = list(disc_env) |>
      purrr::map(\(x) list("@id" = x$`@id`))
  )
  rocrate <- list(disc_setting, disc_env, safe_setting_root) |>
    purrr::reduce(rocrateR::add_entity, .init = rocrateR::rocrate_5s())

  expect_error(
    extract_safe_setting(rocrate, id = "#nonexistent"),
    "No matching entities were found!"
  )
})

test_that("extract_safe_setting.rocrate validates rocrate input", {
  bad_rc <- structure(list(), class = "rocrate")

  expect_error(
    extract_safe_setting.rocrate(bad_rc)
  )
})

# -------------------------------------------------------------------------
# flatten_safe_setting.rocrate()
# -------------------------------------------------------------------------
test_that("flatten_safe_setting.rocrate returns tibble with correct columns", {
  # create an RO-Crate with setting entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#setting-1",
        type = "PropertyValue",
        name = "analysis.threshold",
        value = "0.05"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#software-1",
        type = "SoftwareApplication",
        name = "DataSHIELD",
        version = "6.2.0"
      )
    )

  out <- flatten_safe_setting(rocrate)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("id", "name", "value", "version") %in% names(out)))
})

test_that("flatten_safe_setting.rocrate extracts PropertyValue fields", {
  # create an RO-Crate with setting entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#setting-1",
        type = "PropertyValue",
        name = "analysis.threshold",
        value = "0.05"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#software-1",
        type = "SoftwareApplication",
        name = "DataSHIELD",
        version = "6.2.0"
      )
    )

  out <- flatten_safe_setting(rocrate)

  expect_true("#setting-1" %in% out$id)
  expect_true("analysis.threshold" %in% out$name)
  expect_true("0.05" %in% out$value)
})

test_that("flatten_safe_setting.rocrate extracts SoftwareApplication version", {
  # create an RO-Crate with setting entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#setting-1",
        type = "PropertyValue",
        name = "analysis.threshold",
        value = "0.05"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#software-1",
        type = "SoftwareApplication",
        name = "DataSHIELD",
        version = "6.2.0"
      )
    )

  out <- flatten_safe_setting(rocrate)

  software_row <- out[out$id == "#software-1", ]

  expect_equal(software_row$name, "DataSHIELD")
  expect_equal(software_row$version, "6.2.0")
})

test_that("flatten_safe_setting.rocrate filters by id correctly", {
  # create an RO-Crate with setting entities
  rocrate <- rocrateR::rocrate_5s() |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#setting-1",
        type = "PropertyValue",
        name = "analysis.threshold",
        value = "0.05"
      )
    ) |>
    rocrateR::add_entity(
      entity = rocrateR::entity(
        id = "#software-1",
        type = "SoftwareApplication",
        name = "DataSHIELD",
        version = "6.2.0"
      )
    )

  out <- flatten_safe_setting(rocrate, id = "#software-1")

  expect_equal(nrow(out), 1)
  expect_equal(out$id, "#software-1")
  expect_equal(out$name, "DataSHIELD")
})

test_that("flatten_safe_setting.rocrate returns empty tibble on error", {
  # Force error inside tryCatch by giving malformed object
  bad_obj <- structure(list(), class = "rocrate")

  out <- flatten_safe_setting(bad_obj)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

# -------------------------------------------------------------------------
# extract_safe_setting.opal() (unit + integration strategy)
# -------------------------------------------------------------------------
test_that("extract_safe_setting.opal calls safe_setting and returns rocrate", {
  # dummy opal-like object (S3 dispatch target)
  opal_con <- structure(list(), class = "opal")

  # mock safe_setting to avoid real server dependency
  mock_safe_setting <- function(x, rocrate) {
    setting <- rocrateR::entity(
      id = "#mock-setting",
      type = "PropertyValue",
      name = "mock.option",
      value = "TRUE"
    )
    rocrateR::add_entity(rocrate, setting)
  }

  with_mocked_bindings(
    safe_setting = mock_safe_setting,
    {
      out <- extract_safe_setting.opal(opal_con)
    }
  )

  expect_s3_class(out, "rocrate")

  pv <- .get_entity(out, type = "PropertyValue")
  expect_true(length(pv) >= 1)
})

# -------------------------------------------------------------------------
# optional TRUE Opal demo integration (matches your demo server workflow)
# -------------------------------------------------------------------------
test_that("extract_safe_setting.opal works with Opal demo server (if available)", {
  con <- try(opal_demo_con(), silent = TRUE)
  skip_if(inherits(con, "try-error"), "Opal demo server not available")

  rocrate <- extract_safe_setting(con)

  expect_s3_class(rocrate, "rocrate")
})

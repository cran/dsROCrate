#' Load `content` from external files
#'
#' @param rocrate Object with the [rocrate][rocrateR::rocrate()] class.
#' @param roc_path String with path to the root of the RO-Crate.
#'
#' @returns Update `rocrate` object.
#' @keywords internal
load_content <- function(rocrate, roc_path) {
  # get 'File' entities with missing `content` (if any)
  file_ents <- rocrate |>
    .get_entity(type = "File") |>
    Filter(f = function(x) is.null(getElement(x, "content")))
  ## attempt loading contents, if any entities were found
  for (ent in file_ents) {
    # attempt loading the contents from the filenames given by `@id`
    content <- tryCatch(
      {
        if (getElement(ent, "encodingFormat") == "text/csv") {
          utils::read.csv(file.path(roc_path, getElement(ent, "@id")))
        } else {
          readLines(file.path(roc_path, getElement(ent, "@id")))
        }
      },
      error = function(e) {
        NULL
      },
      warning = function(e) {
        NULL
      }
    )

    # update entity within the RO-Crate
    if (!is.null(content)) {
      rocrate <- rocrate |>
        rocrateR::add_entity_value(
          id = getElement(ent, "@id"),
          key = "content",
          value = list(content)
        )
    }
  }

  return(rocrate)
}

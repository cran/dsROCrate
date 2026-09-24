#' @export
as.data.frame.safe_call <- function(x, ...) {
  data.frame(
    id = x$id,
    timestamp = format(x$created_at, '%Y-%m-%dT%H:%M:%S'),
    user = x$user,
    r_cmd = x$original,
    fx = paste0(x$package, x$namespace, x$fx),
    args = I(list(x$args)),
    session = x$session,
    profile = x$profile,
    stringsAsFactors = FALSE
  )
}

#' Create new `safe_call` object
#'
#' @param original Original function call.
#' @param package Function's package.
#' @param namespace Function's namespace.
#' @param fx Function's name.
#' @param args List with arguments.
#' @param id Unique call ID.
#' @param ... Additional arguments.
#'
#' @returns New `safe_call` object.
#' @keywords internal
#'
#' @noRd
new_safe_call <- function(
  original,
  package,
  namespace,
  fx,
  args,
  id = uuid::UUIDgenerate(),
  ...
) {
  stopifnot(is.character(original))
  stopifnot(is.character(fx))
  stopifnot(is.list(args))

  structure(
    list(
      id = id,
      original = original,
      package = package,
      namespace = namespace,
      fx = fx,
      args = args,
      created_at = getElement(list(...), "@timestamp"),
      user = getElement(list(...), "username"),
      session = getElement(list(...), "ds_id"),
      profile = getElement(list(...), "ds_profile")
    ),
    class = "safe_call"
  )
}

#' Safe call details
#'
#' @param call Object with function call.
#' @param ... Additional arguments.
#'
#' @returns Object with the class `safe_call`.
#' @keywords internal
#'
#' @noRd
safe_call <- function(call, ...) {
  UseMethod("safe_call")
}

#' @export
safe_call.character <- function(call, ...) {
  expr <- str2lang(call)
  safe_call(expr, ...)
}

#' @export
safe_call.call <- function(call, ...) {
  parsed <- parse_call(call)

  do.call(new_safe_call, c(parsed, list(...)))
}

#' @export
as.data.frame.safe_call <- function(x, ...) {
  data.frame(
    timestamp = format(x$created_at, '%Y-%m-%dT%H:%M:%S'),
    user = x$user,
    r_cmd = x$original,
    fx = paste0(x$package, x$namespace, x$fx),
    args = x$args,
    session = x$session,
    profile = x$profile,
    stringsAsFactors = FALSE
  )
}

new_safe_call <- function(
  original,
  package,
  namespace,
  fx,
  args,
  ...
) {
  stopifnot(is.character(original))
  stopifnot(is.character(fx))
  stopifnot(is.list(args))

  structure(
    list(
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

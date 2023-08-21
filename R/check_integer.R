#' Checks a set of variables can be coerced to integer and coerces them
#' 
#' N.B. This only works for the specific environment (to prevent weird side effects)
#'
#' @param ... a list of symbols
#' @param .message a glue spec containing `{param}` as the name of the parameter and `{err}` the cause fo the error
#' @param .env the environment to check (defaults to calling environment)
#'
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#'
#' @examples
#' a = c(1:4)
#' b = c("1",NA,"3")
#' f = NULL
#' g = NA
#' check_integer(a,b,f,g)
#' 
#' c = c("dfsfs")
#' e = c(1.0,2.3)
#' try(check_integer(c,d,e, mean))
check_integer = function(..., .message="`{param}` is not an integer ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.integer(.x)
  convert = function(tmp) {
    x = tryCatch(
    as.numeric(tmp), 
      error = function(e) stop("error casting to numeric"),
      warning = function(w) stop("non numeric format")
    )
    if (any(stats::na.omit(abs(x-round(x)) > .Machine$double.eps^0.5))) stop("rounding detected") 
    return(as.integer(x))
  }
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}


# TODO: named arguments as parameters for convert
.check_framework = function(..., predicate, convert, .message, .env) {
  vars = rlang::ensyms(...)
  env = .env
  errors = character()
  predicate = purrr::as_mapper(predicate)
  convert = purrr::as_mapper(convert)
  for (i in seq_along(vars)) {
    # i = 1
    focus = rlang::as_name(vars[[i]])
    if (!exists(focus,envir = env, inherits = FALSE)) {
      errors = c(errors,sprintf("'%s' is not defined in this context", focus))
    } else {
      tmp = get(focus,envir=env,inherits = FALSE)
      if (!is.null(tmp)) {
        if (is.function(tmp)) {
          errors = c(errors,sprintf("'%s' is a function", focus))
        } else {
          if (!predicate(tmp)) {
            tmp2 = try(convert(tmp),silent = TRUE)
            if (inherits(tmp2, "try-error")) {
              param = focus
              err = attr(tmp2,"condition")$message
              errors = c(errors,glue::glue(.message))
            } else {
              env[[focus]] = tmp2
            }
          }
        }
      }
    }
  }
  if (length(errors) > 0) stop(paste0(1:length(errors), ") ", errors,collapse = "\n"))
}

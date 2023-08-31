#' Checks a set of variables can be coerced to numeric and coerces them
#' 
#' N.B. This only works for the specific environment (to prevent weird side effects)
#'
#' @inheritParams check_integer
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#'
#' @examples
#' a = c(1:4L)
#' b = c("1",NA,"3.3")
#' f = NULL
#' g = NA
#' check_numeric(a,b,f,g)
#' 
#' c = c("dfsfs")
#' try(check_numeric(c,d, mean))
check_numeric = function(..., .message="`{param}` is non-numeric ({err}).", .env = rlang::caller_env()) {
  predicate = ~ is.numeric(.x)
  convert = function(tmp) {
    x = tryCatch(
      as.numeric(tmp), 
      error = function(e) stop("error casting to numeric"),
      warning = function(w) stop("non numeric format")
    )
    return(x)
  }
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}
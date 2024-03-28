#' Checks a set of variables can be coerced to a date and coerces them
#' 
#' @inheritParams check_integer
#' @inheritDotParams base::as.Date
#' @return nothing. called for side effects. throws error if not all variables can be coerced.
#' @export
#' 
#' @concept params_check
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
check_date = function(..., .message="`{param}` is not a date: ({err}).", .env = rlang::caller_env()) {
  
  predicate = ~ lubridate::is.Date(.x)
  
  convert = function(tmp, ...) {
    x = tryCatch(
      as.Date(tmp, ...), 
      error = function(e) stop("error casting to date: ",e$message),
      warning = function(w) stop("non compatible date format")
    )
    return(x)
  }
  .check_framework(..., predicate = predicate, convert = convert, .message = .message, .env=.env)
}
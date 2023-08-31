#' Cause warnings to create an error
#'
#' @param expr expression to evaluate
#'
#' @return the evaluated expression of an error
#' @export
#'
#' @examples
#' try(escalateWarnings(as.integer("ASDAS")))
escalateWarnings = function(expr) {
  withCallingHandlers(
    expr, 
    warning = function(w) stop(w$message,call. = FALSE)
  )
}
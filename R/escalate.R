#' Cause warnings to create an error
#' 
#' The opposite of `suppressWarnings()`. This will immediately error if a warning
#' if thrown by `expr`. This is useful to track down the source of a random
#' and to prevent Rs permissive approach to data transformations. It is also
#' useful to identify where in the code a intermittent `rlang` warning is being
#' issued once every 8 hours.
#'
#' @param expr expression to evaluate
#'
#' @return the evaluated expression or an error
#' @export
#'
#' @examples
#' try(escalate(as.integer("ASDAS")))
#' 
#' try(escalate(rlang::warn("test", .frequency="regularly", .frequency_id = "asdasdasasdd")))
#' try(escalate(rlang::warn("test", .frequency="regularly", .frequency_id = "asdasdasasdd")))
#' try(escalate(rlang::warn("test", .frequency="regularly", .frequency_id = "asdasdasasdd")))
#' try(escalate(rlang::warn("test", .frequency="regularly", .frequency_id = "asdasdasasdd")))
#' 
#' # options("rlib_warning_verbosity"=NULL)
#' # options("rlib_warning_verbosity"="verbose")
#' # "lifecycle_verbosity"="warning"
escalate = function(expr) {
  opt = options(
    "rlib_warning_verbosity"="verbose", 
    "warn"=2
  )
  on.exit(options(opt))
  withCallingHandlers(
    expr, 
    warning = function(w) {
      stop(w$message,call. = FALSE)
    },
    error = function(e) {
      stop(e$message,call. = FALSE)
    }
  )
}
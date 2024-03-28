#' Fully evaluate the arguments from a function call as a named list.
#'
#' Used within a function this provides access to the actual arguments provided
#' during invocation of the parent function, plus any default values. The
#' parameters are evaluated eagerly before being returned (so symbols and expressions
#' must resolve to real values.)
#'
#' @param env the environment to check (default `rlang::caller_env()`)
#' @param missing include missing parameters in list (default `TRUE`)?
#'
#' @return a named list of the arguments of the enclosing function
#' @export
#'
#' @examples
#'
#' ftest = function(a,b,c="default",...) {
#'   tmp = get_fn_args()
#'   tmp
#' }
#'
#' ftest(a=1, b=2)
#'
#' # missing param `b` - empty values are returned just as a name in the environment
#' # with no value but which can be checked for as if in the environment.
#' tmp = ftest(a=1)
#' class(tmp$b)
#' rlang::is_missing(tmp$b)
#' b = 1
#' rlang::is_missing(tmp$b)
#'
#' # extra param `d` and default parameter `c`
#' ftest(a=1, b=2, d="another")
#'
#' # does not work
#' try(ftest(a=1, b=2, d=another))
#' # does work
#' tmp = ftest( a=1, d= as.symbol("another") )
#' # also does work
#' another =5
#' ftest( a=1, d= another)
#' 
#' # Filter out missing values
#' 
#' ftest2 = function(a,b,c="default",...) {
#'   tmp = get_fn_args(missing=FALSE)
#'   tmp
#' }
#' 
#' ftest2(a=1)
#' 
get_fn_args = function(env = rlang::caller_env(), missing=TRUE) {
  tmp = rlang::as_quosure(str2lang("rlang::list2(...)"), env = env)
  dots = tryCatch(rlang::eval_tidy(tmp,env = env), error=function(e) list())
  out = c(as.list(env), dots)
  if (!missing) out = out[!sapply(out,rlang::is_missing)]
  return(out)
}
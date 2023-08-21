
#' Resolve missing values in function parameters
#'
#' Uses relationships between parameters to iteratively fill in missing values.
#' It is possible to specify an inconsistent set of rules or data in which case
#' the resulting values will be picked up and an error thrown.
#'
#' @param ... a set of relationships as a list of `x=y+z` expressions
#' @param .env the environment to check in (optional - defaults to `caller_env()`)
#'
#' @return nothing. Alters the `.env` environment to fill in missing values or 
#'  throws an informative error
#' @export
#'
#' @examples
#' # missing variables left with default value of NULL in function definition
#' testfn = function(pos = NULL, neg=NULL, n=NULL) {
#'   resolve_missing(pos=n-neg, neg=n-pos, n=pos+neg)
#'   return(tibble::tibble(pos=pos,neg=neg,n=n))
#' }
#' 
#' testfn(pos=1:4, neg = 4:1)
#' testfn(neg=1:4, n = 10:7)
#' 
#' # not enough info to infer the missing variables
#' try(testfn(neg=1:4))
#' 
#' # the parameters given are inconsistent with the relationships defined.
#' try(testfn(pos=2, neg=1, n=4))
resolve_missing = function(..., .env = rlang::caller_env()) {
  exprs = rlang::enexprs(...)
  env = .env
  #TODO: check in case everything is null
  for (k in seq_along(exprs)) {
    deferred = FALSE
    for (i in seq_along(exprs)) {
      # i = 1
      focus = names(exprs)[[i]]
      if (focus != "") {
        tmp = env[[focus]]
        if (is.null(tmp)) {
          params = all.vars(exprs[[i]])
          # This version will evaluate when NULL values are present
          # if (all(sapply(params, function(x) exists(x,env,inherits = FALSE)))) {
          # This will not:
          if (!any(sapply(params, function(x) is.null(env[[x]])))) {
            env[[focus]] = eval(exprs[[i]], env)
          } else {
            deferred = TRUE
          }
        }
      }
    }
    if (!deferred) {
      # check the consistency of the results
      check_consistent(..., .env=env)
      return(invisible(NULL))
    }
  }
  if (deferred) {
    exprs = exprs[names(exprs) != ""]
    mentioned = unique(unname(unlist(lapply(exprs, all.vars)))) 
    isnl = sapply(mentioned, function(x) is.null(env[[x]]))
    stop("unable to infer missing variables: ",
         paste0("`",names(isnl[isnl]),"`",collapse = ", "),
         " using:\n",
         paste0(names(exprs)," = ",as.character(exprs), collapse = "\n"), 
         "\ngiven variables: ",
         paste0("`",names(isnl[!isnl]),"`",collapse = ", "))
  }
}
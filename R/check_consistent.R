# pos = 1:10
# n = 10:1
# neg = n+pos
# check_consistent(pos=n-neg, neg=n-pos, n=pos+neg)
# check_consistent(pos=n-neg, neg=n-pos, n=pos+neg, n>pos, n>neg, x=pos+neg)
#' Check function parameters are conform to a set of rules
#' 
#' 
#'
#' @param ... a set of rules to check either as `x=y+z`, or `x>y`. Single `=`
#'   assignment is checked for equality using `identical` otherwise the
#'   expressions are evaluated and checked they all are true. This for
#'   consistency with `resolve_missing` which only uses assignment, and ignores
#'   logical expressions.
#' @param .env the environment to check in 
#'
#' @return nothing, throws an informative error if the checks fail.
#' @export
#'
#' @examples
#' testfn = function(pos=NULL, neg=NULL, n=NULL) {
#'   check_consistent(pos=n-neg, neg=n-pos, n=pos+neg, n>pos, n>neg)
#' }
#' 
#' testfn(pos = 1:4, neg=4:1, n=rep(5,4))
#' try(testfn(pos = 1:4, neg=5:2, n=rep(5,4)))
check_consistent = function(..., .env = rlang::caller_env()) {
  exprs = rlang::enexprs(...)
  env = .env
  errors = character()
  for (i in seq_along(exprs)) {
    # i = 1
    rhs = format(exprs[[i]])
    focus = names(exprs)[[i]]
    if (focus != "") tmp = env[[focus]] else tmp = NaN
    tmp2 = try( eval(exprs[[i]], env), silent = TRUE )
    if (inherits(tmp2, "try-error")) 
      errors = c(errors,sprintf("error '%s' evaluating constraint: '%s'", attr(tmp2,"condition")$message, rhs))
    else if (focus == "") { 
      if(!isTRUE(all(tmp2))) errors = c(errors,sprintf("constraint '%s' is not met", rhs))
    } else {
      if (is.null(tmp) && !is.null(tmp2)) 
        errors = c(errors,sprintf("'%s' is NULL, in expression: '%s = %s'", focus, focus, rhs))
      else if (is.null(tmp2) && !is.null(tmp)) 
        errors = c(errors,sprintf("'%s' is NULL, in expression: '%s = %s'", rhs, focus, rhs))
      else if (!identical(tmp, tmp2) && !isTRUE(all.equal(tmp,tmp2))) 
        errors = c(errors,sprintf("constraint '%s = %s' is not met.", focus, rhs))
    }
  }
  if (length(errors) > 0) stop(paste0(1:length(errors), ") ", errors,collapse = "\n"))
}

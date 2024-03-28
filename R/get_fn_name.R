#' Get the name of a function
#' 
#' Functions may be named or anonymous. When functions are used as a parameter, 
#' for error reporting it is sometimes useful to be able to refer to the function 
#' by the name it is given when it is defined. Sometimes functions can have multiple
#' names.
#'
#' @param fn a function definition (defaults to the function from which 
#'  `get_fn_name` is called)
#' @param fmt passed to `sprintf` with the function name e.g. `%s()` will append
#'   brackets
#' @param collapse passed to paste0 in the case of multiple matching functions. 
#'   set this to `NULL` if you want the multiple function names as a vector. 
#'
#' @return the name of the function or `"<unknown>"` if not known
#' @export
#'
#' @examples
#' # detecting the name when function used as a parameter. This is the 
#' # primary use case for `get_fn_name`
#' testfn2 = function(fn) {
#'   message("called with function: ",get_fn_name(fn))
#' }
#' 
#' testfn2(mean)
#' testfn2(utils::head)
#' testfn2(testfn2)
#' 
#' # detecting the name of a calling function, an unusual use case as this is
#' # normally known to the user.
#' testfn = function() {
#'   message(get_fn_name(fmt="%s(...)")," is a function")
#' }
#' 
#' `test fn 2` = testfn
#' test_fn_3 = testfn
#' testfn()
get_fn_name = function(fn = rlang::caller_fn(), fmt="%s", collapse="/") {
  if (is.null(fn)) return("<unknown>")

  # match the function to functions in its calling environment by its hash code.  
  fnenv = as.list(rlang::fn_env(fn))
  fnenv = fnenv[sapply(fnenv,is.function)]
  fndigest = suppressWarnings(lapply(fnenv, digest::digest))
  matches = sapply(fndigest, function(x) isTRUE(all.equal(x,digest::digest(fn))))
  fnenv = fnenv[matches]
  
  if (any(matches)) {
    
    ns = stringr::str_remove(lapply(lapply(fnenv,rlang::fn_env),rlang::env_name),"^namespace:")
    tmp = trimws(ifelse(!ns %in% c("global","base",""), sprintf("%s::%s",ns,names(fnenv)), names(fnenv)))
    ok = stringr::str_detect(tmp, "^([a-zA-Z_]+::){0,1}[a-zA-Z_][a-zA-Z0-9_]+$")
    tmp[!ok] = paste0("`",tmp[!ok],"`")
    tmp = sprintf(fmt,tmp)
    if (!is.null(collapse)) tmp = paste0(tmp,collapse = collapse)
    return(tmp)
    
  }
  return("<unknown>")
}



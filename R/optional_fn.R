#' Get an optional function without triggering a CRAN warning
#'
#' You want to use a function if it is installed but don't want it to
#' be installed as part of your package and you don't want to reference it
#' as part of the Imports or Suggests fields in a package DESCRIPTION.
#'
#' @param pkg the package name (or the function name as `"pkg::fn"`)
#' @param name the function you wish to use (if not specified in `pkg`)
#' @param alt an alternative function that can be used if the requested one is
#'   not available. The default throws an error if the package is not available,
#'   but a fallback can be used instead.
#'
#' @return the function you want if available or the alternative
#' @export
#'
#' @examples
#' # use openSSL if installed:
#' fn = optional_fn("openssl", "md5", alt = digest::digest)
#' as.character(fn(as.raw(c(1,2,3))))
#' 
#' fn2 = optional_fn("asdasdadsda::asdasdasd")
#' try({
#'   # block to execute if asdasdadsda::asdasdasd() is available
#'   fn2()
#' }) 
#' # add silent=TRUE to hide error message
#' 
#' # this function does not exists and so the alternative is used instead.
#' fn3 = optional_fn("asdasdadsda::asdasdasd", ~ message("formula alternative"))
#' fn3()
#' 
optional_fn = function(pkg, name, alt=function(...) {stop("function `",pkg,"::",name,"(...)` not available")}) {
  if (rlang::is_missing(name) || rlang::is_function(name) || rlang::is_formula(name)) {
    if (rlang::is_function(name) || rlang::is_formula(name)) alt = name
    tmp = unlist(stringr::str_split(pkg,stringr::fixed("::")))
    if (length(tmp) != 2) stop("either `name` must be supplied or the format `package::function` used for the `pkg` parameter")
    pkg = tmp[1]
    name = tmp[2]
  }
  if (!rlang::is_installed(pkg)) {
    alt = rlang::as_function(alt)
    return(alt)
  }
  return(utils::getFromNamespace(name, pkg))
}


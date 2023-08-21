#' Strictly recycle function parameters 
#' 
#' `recycle` is called within a function and ensures the parameters
#' in the calling function are all the same length by repeating them using `rep`.
#' This function alters the environment from which it is called and so should be
#' used with caution. It is stricter than R recycling in that it will not repeat
#' vectors other than length one to match the longer ones, and it throws more 
#' informative errors.
#'
#' @param ... the variables to recycle
#' @param .min the minimum length of the results (defaults to 1)
#'
#' @return the length of the longest variable
#' @export
#'
#' @examples
#' testfn = function(a, b, c) {
#'   n = recycle(a,b,c)
#'   print(a)
#'   print(b)
#'   print(c)
#'   print(n)
#' }
#' 
#' testfn(a=c(1,2,3), b="needs recycling", c=NULL)
recycle = function(..., .min=1) {
  names = sapply(rlang::ensyms(...), rlang::as_label)
  dots = rlang::list2(...)
  lengths = sapply(dots, length)
  ml = max(c(lengths,.min))
  
  
  if (!all(lengths %in% c(0,1,ml))) {
    names = names[lengths != 1 & lengths != ml]
    stop(sprintf("%s is/are the wrong lengths. They should be 1 or %d",paste0(names,collapse=",") ,ml))
  }
  
  env = rlang::caller_env()
  
  if (ml>1) {
    for (i in seq_along(dots)) {
      x = dots[[i]]
      name = names[[i]]
      if (length(x) == 1) 
        env[[name]] = rep(x,ml)
    }
  }
  
  return(ml)
  
}
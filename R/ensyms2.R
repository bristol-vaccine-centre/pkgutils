
#' Convert a parameter into a list of symbols
#'
#' Used within a function this allows for a list of columns to be given as a
#' parameter to the parent function in a number of flexible ways. A list of
#' unquoted symbols, a list of quoted strings, a tidyselect syntax (assuming the
#' parent function has a dataframe is its first argument) or as a formula.
#'
#' @param x one of a list of symbols, a list of strings, a `tidyselect`
#'   expression, or a formula
#' @param .as the type of output desired:  (`symbol` or `character`)
#' @param .side the desired side of formulae output:  (`lhs` or `rhs`); this is
#'   only relevant if `x` is a formula (or list of formulae)
#' @param .tidy is this being called in the context of a "tidy" style function. I.e.
#'   one that takes a dataframe as the main parameter? (Default is `FALSE`)
#'
#' @return either a list of symbols or a character vector of the symbols
#'
#' @concept var_group
#'
#' @export
#' @examples
#' 
#' # TODO: convert these to tests
#' eg = function(df, vars, ...) {
#'   vars = ensyms2(vars, ..., .tidy=TRUE)
#'   print(vars)
#' }
#'
#' eg(iris, c(Sepal.Width, Species, Sepal.Length))
#' eg(iris, c("Sepal.Width", "Species", "Sepal.Length", "extra"))
#' eg(iris, "Sepal.Width")
#' eg(iris, Sepal.Width)
#' eg(iris, dplyr::vars(Sepal.Width))
#' eg(iris, dplyr::vars(Sepal.Width, Species, Sepal.Length))
#' eg(iris, list(Sepal.Width, Species, Sepal.Length))
#' eg(iris, list("Sepal.Width", "Species", "Sepal.Length"))
#' eg(iris, tidyselect::starts_with("Sepal"))
#' eg(iris, Species ~ Sepal.Width + Sepal.Length)
#' eg(iris, Species ~ Sepal.Width + Sepal.Length, .side = "lhs")
#' eg(iris, . ~ Sepal.Width + Sepal.Length, .side = "lhs")
#' eg(iris, Sepal.Width + Sepal.Length ~ .)
#' eg(iris, c(~ Sepal.Width + Sepal.Length, ~ Petal.Width + Petal.Length))
#' 
#' try(eg(iris, c(~ .)))
#' eg(iris, list(~ Sepal.Width + Sepal.Length, ~ Petal.Width + Petal.Length))
#' 
#' # In a way this shouldn't work, but does:
#' eg(iris, c(~ Sepal.Width + Sepal.Length, Petal.Width + Petal.Length))
#'
#' # injection support:
#' subs = ensyms2(c("Sepal.Width", "Species", "Sepal.Length"))
#' 
#' # this must be injected as a single thing as the parameter x but actually it 
#' # turns out to be just the same as supplying a list of symbols as the bare 
#' # parameter
#' # eg(iris,!!subs)
#' # ensyms2(!!subs)
#' # same as:
#' # eg(iris,subs)
#' # ensyms2(subs)
#' 
ensyms2 = function(x, .as = c("symbol","character"), .side= c("rhs","lhs"), .tidy = FALSE) {
  
  .as = match.arg(.as)
  .side = match.arg(.side)
  env = rlang::caller_env()
  fn = rlang::caller_fn()
  call= rlang::as_label(rlang::caller_call())

  tidysel = FALSE
  # evaluate x if possible. this will be the case for everything other
  # than a bare expression
  tmp = tryCatch({
    rlang::with_options(
      unlist(force(x)),
      "lifecycle_verbosity"="error" # upgrade tidyselect lifecycle warnings to errors.
    )
  }, error = function(e) {
    if(any(e$trace$namespace == "tidyselect") && isFALSE(.tidy))
      stop("tidyselect syntax is not supported in the function call: `",call,"`")
    tmp2 = rlang::enexpr(x)
    
    # If ensyms2 is within another function AND the expression it is
    # called with matches a formal parameter of that function THEN we
    # need to resolve the expression the parent function was called with.
    # to do that we evaluate a "rlang::enexpr()" call in the parent function
    # environment.
    if (!is.null(fn) && format(tmp2) %in% names(formals(fn))) {
      expr = str2expression(sprintf("rlang::enexpr(%s)",format(tmp2)))
      tmp2 = eval(expr,env)
    }
    
    # either way this results in a call object
    return(tmp2)
  })
  
  if (!isFALSE(.tidy)) {
    if (is.data.frame(.tidy)) {
      .df = .tidy
      .tidy = TRUE
    } else {
      .df = .search_call_stack()
      .tidy = TRUE
    }
  }
  
  out2 = character()
  if (is.null(tmp)) {
    out = character()
  } else if (inherits(tmp, "call")) {
    out = tryCatch({
      # try and evaluate parameter x as a tidyselect call
      # by looking for a dataframe on the call stack.
      names(tidyselect::eval_select(tmp, .df))
    }, error = function(e) {
      # Otherwise we just try and extract the vars from the
      # call. This should work for simple lists of symbols.
      all.vars(tmp)
    })
    
  } else {
    
    if (!is.list(tmp)) tmp = list(tmp)
    
    if (length(tmp)==0) {
      out = character()
    } else if (tmp %>% .list_is(is.character)) {
      out = unlist(tmp)
    } else if (tmp %>% .list_is(is.symbol)) {
      out = sapply(tmp, rlang::as_label)
    } else if (tmp %>% .list_is(inherits, "formula")) {
      
      # all quosures are also formulae
      rhs = lapply(tmp, rlang::f_rhs)
      out = unique(unlist(lapply(rhs, all.vars)))
      
      # they never have lhs though and 
      if (tmp %>% .list_is(inherits, "quosure")) {
        out2 = character()
      } else {
        lhs = lapply(tmp, rlang::f_lhs)
        out2 = unique(unlist(lapply(lhs, all.vars)))
      }
      
      
      
    } else {
      stop("Not sure what to do with : ",paste0(sapply(tmp, class), collapse = ","))
    }
  } 
  
  # handle dot in formulae if a tidy
  if (.tidy) {
    
    grps = .df %>% dplyr::group_vars()
    if (all(out == ".")) {
      out = colnames(.df) %>% setdiff(grps) %>% setdiff(out2)
    }
    
    if (all(out2 == ".")) {
      out2 = colnames(.df) %>% setdiff(grps) %>% setdiff(out)
    }
  }
  out = out[out != "."]
  out2 = out2[out2 != "."]
  
  # decide on output type
  if (.side == "lhs") out = out2
  if (.as == "symbol") 
    return(lapply(unique(out), as.symbol))
  else
    return(unique(out))
}

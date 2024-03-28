

#' Extract a definition of column groups from function parameters
#'
#' This is a supporting utility for functions that have a signature of
#' `function(df, ...)` that operate on different groups of columns, and need the
#' user to supply column groups in a simple way. There are 2 or 3 levels of
#' column grouping that can be specified easily in this style of function, and
#' they are generally referred to as `z` (i.e. group, or cohort), `y` (i.e.
#' subgroup, or response) and `x` (i.e. data). In some configurations,
#' only `z` and `x` are available.
#'
#' @param df a data frame which may be grouped
#' @param .infer_y if only `z` and `x` is defined make `y` the rest of the dataframe columns
#' @param ... a specification for the groupings which may be one of:
#' * A formula or list of formulae (e.g. `y1 + y2 ~ x1 + x2`, z:from df grouping).
#' the `.` can be used to specify the rest of the columns, e.g. `y1 + y2 ~ .`
#' * A list of symbols (`x1, x2, ...`, z:from df grouping, y:empty)
#' * A list of quosures (e.g. `dplyr::vars(x1,x2)`) (x, z:from df grouping, y:empty)
#' * One tidyselect specification (x, z:from df grouping, y:empty)
#' * Two tidyselect specifications (x, y, z:from df grouping)
#' * Three tidyselect specifications (x, y, z, N.B. df must be ungrouped for this to work)
#' * Column names as strings (x, z:from df grouping, y:empty)
#'
#' @concept var_group
#'
#' @return a `var_grp_df` with defined `z`, `y` and `x` column groups, for use
#'   within the `var_group_*` framework.
#' @export
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Sepal.Width)
#' 
#' tmp = iris %>% dplyr::group_by(Species) %>% 
#'   var_group(tidyselect::starts_with("Sepal"),tidyselect::starts_with("Petal"))
var_group = function(df, ..., .infer_y = FALSE) {
  
  # most likely this will be the z. This takes precendence over a third
  # tidyselect specification.
  z = df %>% dplyr::group_vars()
  
  # evaluate the dots. 
  # if it is a tidyselect style spec there will be an error
  tmp = tryCatch({
    rlang::with_options(
      unlist(c(...)),
      "lifecycle_verbosity"="error" # upgrade tidyselect lifecycle warnings to errors.
    )
  }, error=function(e) rlang::enexprs(...))
  
  if (is.null(tmp)) return(as.var_grp_df(df, z, list(), list()))
  
  if (!is.list(tmp)) tmp = list(tmp)
  
  out2 = NULL
  if (length(tmp)==0) {
    
    out = character()
  
  } else if (length(unique(sapply(tmp,function(x) class(x)[[1]])))>1) {
    
    rlang::abort(c(
      "`...` is not correctly specified. It must be either formulae, tidyselect or list of columns, but not a mix",
      "*" = "this can happen if you forget to enclose a tidyselect specification in `c(...)`"
    ), call = rlang::caller_env())
    
  } else if (tmp %>% .list_is(is.character)) {
    # bare character array
    out = tmp
    
  } else if (tmp %>% .list_is(is.symbol)) {
    out = sapply(tmp, rlang::as_label)
    
  } else if (tmp %>% .list_is(inherits, "formula")) {
    # a dplyr::vars(x,y,z)
    rhs = lapply(tmp, rlang::f_rhs)
    out = unique(unlist(lapply(rhs, all.vars)))
    
    lhs = lapply(tmp, function(x) tryCatch(rlang::f_lhs(x), error = function(e) NULL))
    if (!.list_is(lhs,is.null)) out2 = unique(unlist(lapply(lhs, all.vars)))
    
    if (any(out == ".")) {
      out = out[out != "."]
      out = unique(c(out, setdiff(colnames(df), c(z,out2))) )
    }
    
    if (any(out2 == ".")) {
      out2 = out[out2 != "."]
      out2 = unique(c(out2, setdiff(colnames(df), c(z,out))) )
    }
    
  } else if (tmp %>% .list_is(inherits, "call") || tmp %>% .list_is(inherits, "function")) {
    # a tidyselect function call or a bare c(a,b,c)
    out = names(tidyselect::eval_select(tmp[[1]], df))
    
    if (length(tmp) == 2) {
      # reverser
      out2 = names(tidyselect::eval_select(tmp[[1]], df))
      out = names(tidyselect::eval_select(tmp[[2]], df))
    }
    
    if (length(tmp) == 3) {
      if (length(z) > 0) stop("too many tidyselect conditions in `var_group()`",call. = FALSE)
      z = names(tidyselect::eval_select(tmp[[1]], df))
      out2 = names(tidyselect::eval_select(tmp[[2]], df))
      out = names(tidyselect::eval_select(tmp[[3]], df))
    }
    if (length(tmp) > 3 ) stop("too many tidyselect conditions in `var_group()`",call. = FALSE)
    
  }
  
  if (is.null(out2)) {
    if (.infer_y) {
      out2 = setdiff(colnames(df), c(z,out))
    } else {
      out2 = character()
    }
  }
  
  as.var_grp_df(df, z, out2, out)
  
}

# empty list always returns TRUE
.list_is = function(l, .f, ...) {
  .f = rlang::as_function(.f)
  return(all(sapply(l, .f, ...)))
}

f = function(...) {
  tmp = tryCatch({
    unlist(c(...))
  }, error=function(e) rlang::enexprs(...))
  print(class(tmp))
  print(length(tmp))
  if (inherits(tmp,"list")) {
    print(paste0(sapply(tmp, class),collapse = ", "))
    # if (all(sapply(tmp, inherits, "formula"))) {
    #   print("formula")
    # } else if (all(sapply(tmp, inherits, "expression")))
  }
}

# f(x,y,z)
# f(list(x,y,z))
# f(c(x,y,z), c(a,b,c))
# f(list("x","y","z"))
# f(c("x","y","z"))
# f(c(~ x, ~ y, ~ z))
# f(~ x + y + z)
# f(~ x + y, z + a)
# f(~ .)
# f(list(~ x + y + z))
# f(tidyselect::everything())
# f(dplyr::vars(a,b,c))
# v = lapply(c("a","b","c"), as.symbol)
# f(!!!v)


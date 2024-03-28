#' The `var_grp_df` dataframe subtype
#' 
#' This is like a grouped data frame but with 3 grouping dimensions. These are
#' labelled z, y, and x and relate to as `z` (i.e. group, or cohort), `y` (i.e.
#' subgroup, or response) and `x` (i.e. data). In some configurations,
#' only `z` and `x` are non-empty. The purpose of this is to make some group / subgroup
#' data operations consistent. An example is running multiple models across
#' different bootstraps from example.
#' 
#' @param df a dataframe
#'
#' @param z the z columns (e.g. cohort) as a list of columns
#' @param y the y columns (e.g. response) as a list of columns 
#' @param x the x columns (e.g. predictor) as a list of columns 
#'
#' @export
#' @concept var_group
#' 
#' @examples
#' tmp = as.var_grp_df(iris, 
#'   c("Species"), 
#'   c("Sepal.Width", "Sepal.Length"), 
#'   c("Petal.Width", "Petal.Length"))
#' # print.var_grp_df(tmp)
#' glimpse.var_grp_df(tmp)
#' 
#' tmp2 = as.var_grp_df(iris, 
#'   c(Species), 
#'   tidyselect::starts_with("Sepal"), 
#'   tidyselect::starts_with("Petal"))
#' glimpse.var_grp_df(tmp2)
as.var_grp_df = function(df, z, y, x) {
  
  df = tibble::as_tibble(df)
  z = intersect(ensyms2(z),col_syms(df))
  y = intersect(ensyms2(y),col_syms(df))
  x = intersect(ensyms2(x),col_syms(df))
  
  if (
    any(z %in% y) || any(y %in% x) || any(x %in% z)
  ) warning("`var_grp_df` columns cannot be in more than one of z, y or x")
  
  return(structure(
    df,
    z = z,
    y = y,
    x = x,
    class = unique(c("var_grp_df",class(df)))
  ))
}

#' `var_grp_df` S3 Methods
#' 
#' @param x a `var_grp_df` dataframe 
#' @param ... passed to generic functions
#' 
#' @name var_grp_s3
NULL

#' @describeIn var_grp_s3 glimpse
#' @export
#' @exportS3Method pillar::glimpse var_grp_df
glimpse.var_grp_df = function(x, ...) {
  tmp= format.var_grp_df(x,...)
  cat(tmp[[1]],"\n")
  x2 = x
  class(x2) = setdiff(class(x2),"var_grp_df")
  dplyr::glimpse(x2,...)
}

#' @describeIn var_grp_s3 format
#' @export
format.var_grp_df = function(x, ...) {
  v = var_grps(x)
  x2 = x
  class(x2) = setdiff(class(x2),"var_grp_df")
  return(list(
  sprintf("%d group(s): %s.\n(subgroup) %s ~ %s (data)",
    dplyr::n_distinct(dplyr::select(x,!!!attr(x,"z"))),
    .format_symbols(v$z,", "),
    .format_symbols(v$y," + "),
    .format_symbols(v$x," + ")
  ),
  format(x2, ...)
  ))
}

#' @describeIn var_grp_s3 print
#' @export
print.var_grp_df = function(x, ...) {
  tmp= format.var_grp_df(x,...)
  cat(tmp[[1]],sep = "\n")
  cat(tmp[[2]],sep = "\n")
}

#' @describeIn var_grp_s3 is
#' @export
is.var_grp_df = function(x,...) {
  return(inherits(x, "var_grp_df"))
}

#' Export `var_group` metadata as a formula
#' 
#' Produces the `y` and `x` terms of a `var_grp_df` as a formula for
#' potentially using in a model or another `var_group`
#'
#' @param var_grp_df a `var_group` dataframe
#' @concept var_group
#'
#' @return a formula like `y1 + y2 ~ x1 + x2 + ...`
#' @export
var_group_formula = function(var_grp_df) {
  v = var_grps(var_grp_df)
  return(stats::as.formula(sprintf(
    "%s ~ %s",
    .format_symbols(v$y," + "),
    .format_symbols(v$x," + "))))
}



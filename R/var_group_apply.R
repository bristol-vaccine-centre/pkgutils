#' Cross compare subgroups of data to each other
#' 
#' This function helps construct group wise cross-correlation matrices and other between column 
#' comparisons from a dataframe. We assume we have a data with a major grouping
#' and then data columns we wish to compare to each other. We specify the 
#' columns to compare to each other as a formula or as a tidyselect using a `var_grp_df`
#' and using this we use these a set of columns to compare.
#' 
#' Although the examples here are functional we generally expect these to be wrapped 
#' within a function within a package where the comparisons are pre-defined, and
#' the `var_group` framework is hidden from the user.
#'
#' @param var_grp_df a data frame with major and data groupings 
#' @param ... a set of named functions. The functions must take 2 vectors of the 
#'   type of the columns being compared and generate a single result (which may be a
#'   complex S3 object such as a `lm`). Such functions
#'   might be for example be `chisq.test` for factor columns or `cor` for numeric
#'   columns.
#' @param .diagonal should a column be compared with itself? this is usually `FALSE`
#'
#' @return a dataframe containing the major `z` groupings and unique binary
#'   combinations of `y` and `x` columnsas `y` and `x` columns. The named 
#'   comparisons provided in `...` form the other columns. If these are not 
#'   primitive types this will be a list column.
#' @export
#'
#' @concept var_group
#'
#' @examples
#' iris %>% dplyr::group_by(Species) %>% var_group(~ .) %>%
#'   var_group_compare(
#'     correlation = cor
#'   )
#'   
#' ggplot2::diamonds %>% var_group(tidyselect::where(is.factor)) %>% 
#'   var_group_compare(
#'     chi.p.value = ~ stats::chisq.test(.x,.y)$p.value
#'   )
#'   
#'   
var_group_compare = function(var_grp_df, ..., .diagonal=FALSE) {
  
  dots = rlang::list2(...)
  
  vg2 = var_grps(var_grp_df)
  
  tmp = var_group_modify(var_grp_df, .subgroup = FALSE, .f = function(vdf, ...) {
    
    vg = var_grps(vdf)
    .cols_x = vg$x
    .cols_y = vg$y
    if (length(.cols_y) == 0) {
      .cols_y = .cols_x
    }
    
    dfx = vdf %>% dplyr::select(!!!.cols_x)
    dfy = vdf %>% dplyr::select(!!!.cols_y)
    
    err2 = character()
    out2 = dplyr::bind_rows(
      lapply(colnames(dfx), function(xcol) {
        x = dplyr::pull(dfx,xcol)
        dplyr::bind_rows(
          lapply(colnames(dfy), function(ycol) {
            if (.diagonal || xcol != ycol) {
              y = dplyr::pull(dfy,ycol)
              out = tibble::tibble(y = ycol,x = xcol)
              for (name in names(dots)) {
                fn = rlang::as_function(dots[[name]])
                res = try(fn(x, y), silent = TRUE)
                if (inherits(res, "try-error")) {
                  reason = attr(res,"condition")$message
                  err2 <<- c(err2,reason)
                  res = NA
                }
                if (is.atomic(res)) {
                  out = out %>% dplyr::mutate(!!name := res)
                } else {
                  out = out %>% dplyr::mutate(!!name := list(res))
                }
              }
              return(out)
            } else {
              return(NULL)
            }
          })
        )
      })
    )
    if (length(err2) > 0) warning(unique(err2))
    return(out2)
    
  })
  
  return(tmp %>% dplyr::group_by(!!!vg2$z) %>% dplyr::arrange(.data$y,.data$x, .by_group = TRUE) %>% var_group(y ~ x + .))
  
}
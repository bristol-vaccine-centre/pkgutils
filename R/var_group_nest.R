#' Nest a `var_grp_df` by the `z` columns
#'
#' @param var_grp_df the var_grp dataframe
#' @inheritParams tidyr::nest
#' @param .subgroup in the nested data frames also group the `y` columns
#'
#' @return aa nested dataframe with `z`  columns and a `.key` column with 
#'   the `y` and `x` columns nested in it. The nested data will be grouped by
#'   `y` columns.
#' @export
#' @concept var_group
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Sepal.Width)
#' tmp2 = tmp %>% var_group_nest()
var_group_nest = function(var_grp_df, .subgroup = FALSE, .key="data") {
  vg = var_grps(var_grp_df)
  grps = vg$z
  attr(var_grp_df,"z") = list()
  x = var_grp_df %>% dplyr::ungroup() %>% tidyr::nest(.by = c(!!!grps), .key=.key)
  if (.subgroup) x = x %>% dplyr::mutate(data = purrr::map(data, ~ .x %>% dplyr::group_by(!!!vg$y)))
  return(x)
}


#' Nest a `var_grp_df` by the `z` and `y` columns
#' 
#'
#' @param var_grp_df the var_grp dataframe
#' @inheritParams tidyr::nest
#'
#' @return a nested dataframe with `z` and `y` columns and a `.key` column with 
#'   the `x` columns nested in it
#' @export
#' 
#' @concept var_group
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Sepal.Width)
#' tmp2 = tmp %>% var_group_nest()
var_subgroup_nest = function(var_grp_df, .key="data") {
  vg = var_grps(var_grp_df)
  grps = c(vg$z,vg$y)
  attr(var_grp_df,"x") = as.symbol(.key)
  x = var_grp_df %>% dplyr::ungroup() %>% tidyr::nest(.by = c(!!!grps), .key = .key)
  x = x %>% dplyr::mutate(data = purrr::map(data, ~ .x %>% dplyr::group_by(!!!vg$y)))
  return(x)
}

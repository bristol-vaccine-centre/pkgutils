#' The number of major groups (`z` categories) in a `var_grp_df`
#' 
#' @param var_grp_df the var_grp dataframe
#'
#' @concept var_group
#'
#' @return a count of groups
#' @export
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Sepal.Width)
#' tmp %>% var_group_count()
var_group_count = function(var_grp_df) {
  vg = var_grps(var_grp_df)
  n = dplyr::n_distinct(dplyr::select(var_grp_df, !!!vg$z))
  return(n)
}

#' The number of major and sub groups (`z` and `y` categories) in a `var_grp_df`
#' 
#' @param var_grp_df the var_grp dataframe
#' @param .stratified if `TRUE` return the subgroup count stratified by major groups as a dataframe
#'
#' @return a count of groups and subgroups
#' @export
#' @concept var_group
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Sepal.Width)
#' tmp %>% var_subgroup_count()
var_subgroup_count = function(var_grp_df, .stratified=FALSE) {
  vg = var_grps(var_grp_df)
  if (.stratified) {
    n = var_grp_df %>% dplyr::select(!!!vg$z, !!!vg$y) %>%
      dplyr::distinct() %>%
      dplyr::group_by(!!!vg$z) %>% 
      dplyr::summarise(n_subgroups = dplyr::n())
    return(n)
  } else {
    n = dplyr::n_distinct(dplyr::select(var_grp_df, !!!vg$z, !!!vg$y))
    return(n)
  }
}

#' Does this `var_grp_df` have more than one major group? 
#'
#' @param var_grp_df a `var_group` dataframe
#'
#' @concept var_group
#'
#' @return boolean
#' @export
var_has_groups = function(var_grp_df) {
  return(var_group_count(var_grp_df) > 1)
}
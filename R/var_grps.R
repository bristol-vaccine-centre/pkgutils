

#' Extract grouping info frm a `var_grp_df`
#'
#' @param var_grp_df the dataframe
#'
#' @return a list of lists containing the `x`,`y`, and `z` column sets as symbol lists
#' 
#' @concept var_group
#' 
#' @export
var_grps = function(var_grp_df) {
  return(list(
    z = attr(var_grp_df,"z"),
    y = attr(var_grp_df,"y"),
    x = attr(var_grp_df,"x")
  ))
}
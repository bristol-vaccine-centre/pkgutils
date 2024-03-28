#' Column names as symbols
#'
#' @param df a dataframe
#'
#' @return a list of symbols
#' @export
#' 
#' @concept var_group
#'
#' @examples
#' intersect(col_syms(iris), ensyms2(tidyselect::starts_with("S"), .tidy=iris))
col_syms = function(df) {
  return(lapply(colnames(df),as.symbol))
}
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
#' intersect(colsyms(iris), ensyms2(tidyselect::starts_with("S"), .df=iris))
col_syms = function(df) {
  return(lapply(colnames(df),as.symbol))
}
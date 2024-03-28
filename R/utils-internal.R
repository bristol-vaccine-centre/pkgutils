# package depends
# c("tidyselect","rlang") %>% lapply(usethis::use_package)

# look for a dataframe as the first argument of a function in the call stack
# z = function(a) {.search_call_stack()}
# y = function(a) {a}
# x = function(df, a_default="a") {return(y(y(y(z()))))}
# x(iris)
.search_call_stack = function(nframe = sys.nframe()-1) {
  frame = sys.frame(nframe)
  first_arg_name = names(formals(sys.function(nframe)))[[1]]
  try({
    data = suppressWarnings(get(first_arg_name, envir=frame))
    if(is.data.frame(data)) return(data)
  },silent = TRUE)
  nframe = nframe-1
  if (nframe < 1) stop("no data frame found")
  .search_call_stack(nframe)
}

.as_vars = function(tidyselect, data=NULL) {
  expr = rlang::enquo(tidyselect)
  if(is.null(data)) data = .search_call_stack()
  res = tidyselect::eval_select(expr,data)
  lapply(names(res), as.symbol)
}

.as_character_symbols = function(list_sym) {
  sapply(list_sym, rlang::as_label)
}

.format_symbols = function(list_sym, sep=", ") {
  paste0(.as_character_symbols(list_sym),collapse = sep)
}

.sort_symbols = function(symbols) {
  s = order(sapply(symbols, rlang::as_label))
  symbols[s]
}
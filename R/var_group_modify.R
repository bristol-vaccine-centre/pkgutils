#' Apply a function to each `z` group using `group_modify()`
#'
#' @param var_grp_df  the var_grp dataframe
#' @param .f a function with the signature `function(x,y,z,...)` if the default `.subgroup=TRUE`
#' or of the form `function(xy,z,...)` if `.subgroup=FALSE`. If `.subgroup=TRUE`
#' The function will be called once for each group and subgroup with the parameters
#' `x` being the data as a dataframe with usually multiple rows, and `y` and `z`
#' being single row dataframes contianing the current subgroup and group respectively.
#' Is `subgroup=FALSE` then only the major grouping `z` is used and 
#' @inheritParams dplyr::group_modify
#' @inheritDotParams dplyr::group_modify
#' @param .subgroup in the grouped data frames also subgroup by the `y` columns
#' @param .progress shoudl progress be reported with a progress bar.
#'
#' @return the transformed data as a plain dataframe
#' @export
#' @concept var_group
#'
#' @examples
#' tmp = iris %>% dplyr::group_by(Species) %>% var_group(. ~ Petal.Width + Petal.Length)
#' 
#' tmp2 = tmp %>% var_group_modify(
#'   ~ {
#'     Sys.sleep(0.02)
#'     return(.x %>% count())
#'   },
#'   .progress=TRUE
#' )
#' 
#' tmp3 = tmp %>% var_group_modify(~ .x %>% dplyr::count(), .subgroup=FALSE)
#' 
#' # .f with 2 parameters:
#' tmp %>% var_group_modify(
#'   ~ {
#'     return(tibble::tibble(
#'       Sepal.Area = .y$Sepal.Length*.y$Sepal.Width,
#'       Max.Petal.Area = max(.x$Petal.Length*.x$Petal.Width),
#'       n = nrow(.x)
#'     ))
#'   }
#' ) %>% dplyr::filter(n>1)
var_group_modify = function(var_grp_df, .f, ..., .subgroup = TRUE, .progress=FALSE) {
  total = var_subgroup_count(var_grp_df)
  vg = var_grps(var_grp_df)
  .f = rlang::as_function(.f)
  .envir = rlang::current_env()
  if(.progress) cli::cli_progress_bar(total=total)
  .f2 = function(...) {
    if(.progress) cli::cli_progress_update(.envir = .envir)
    .f(...)
  }
  var_grp_df = var_grp_df %>% 
    magrittr::set_attr("z",list()) %>%
    dplyr::group_by(!!!vg$z)
  .g = function(xy, z, ...) {
    if (.subgroup) {
      xy = xy %>% 
        magrittr::set_attr("y",list()) %>%
        dplyr::group_by(!!!vg$y)
      return(xy %>% dplyr::group_modify(.f2, z=z, ...))
    } else {
      xy = xy %>% as.var_grp_df(list(), vg$y, vg$x)
      return(.f2(xy, z, ...) )
    }
  }
  tmp = var_grp_df %>% 
    magrittr::set_attr("z",list()) %>%
    dplyr::group_by(!!!vg$z) %>%
    dplyr::group_modify(.g) %>%
    tibble::as_tibble() 
  y = if(.subgroup) vg$y else list()
  x = col_syms(tmp) %>% setdiff(y) %>% setdiff(vg$z)
  tmp %>% as.var_grp_df(vg$z,y,x)
}
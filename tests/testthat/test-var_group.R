.expect_vars = function(df, z=NULL,y = NULL, x = NULL) {
  
  if (identical(x,".")) x=colnames(df) %>% setdiff(y) %>% setdiff(z)
  if (identical(y,".")) y=colnames(df) %>% setdiff(x) %>% setdiff(z)
  if (identical(z,".")) z=colnames(df) %>% setdiff(y) %>% setdiff(x)
  
  z = if (!is.null(z)) lapply(z,as.symbol) else NULL
  y = if (!is.null(y)) lapply(y,as.symbol) else NULL
  x = if (!is.null(x)) lapply(x,as.symbol) else NULL
  
  v = var_grps(df)
  
  if (!is.null(z)) {testthat::expect_equal(v$z,z)}
  if (!is.null(y)) {testthat::expect_equal(v$y,y)}
  if (!is.null(x)) {testthat::expect_equal(v$x,x)}
  
}

# f(x,y,z)
# f(list(x,y,z))
# f(c(x,y,z), c(a,b,c))
# f(list("x","y","z"))
# f(c("x","y","z"))
# f(c(~ x, ~ y, ~ z))
# f(~ x + y + z)
# f(~ x + y, z + a)
# f(~ .)
# f(list(~ x + y + z))
# f(tidyselect::everything())
# f(dplyr::vars(a,b,c))
# v = lapply(c("a","b","c"), as.symbol)
# f(!!!v)

test_that("formula interface works", {
  z = "Species"
  y = c("Sepal.Length","Petal.Length")
  x = c("Sepal.Width","Petal.Width")
  
  # Fully specified
  iris %>% dplyr::group_by(Species) %>% 
    var_group(Sepal.Length + Petal.Length ~ Sepal.Width + Petal.Width) %>%
    .expect_vars(z,y,x)
  
  # Fully specified order preserved
  iris %>% dplyr::group_by(Species) %>% 
    var_group(Petal.Length + Sepal.Length ~ Petal.Width + Sepal.Width) %>%
    .expect_vars(z,rev(y),rev(x))
  
  # Use of dot on LHS
  iris %>% dplyr::group_by(Species) %>% 
    var_group(. ~ Sepal.Width + Petal.Width) %>%
    .expect_vars(z,y,x)
  
  # Use of dot on RHS
  iris %>% dplyr::group_by(Species) %>% 
    var_group(Sepal.Length + Petal.Length ~ .) %>%
    .expect_vars(z,y,x)
  
  # Ungrouped => no Z
  iris %>% 
    var_group(Sepal.Length + Petal.Length ~ Sepal.Width + Petal.Width) %>%
    .expect_vars(character(),y,x)
  
  # No LHS
  iris %>% dplyr::group_by(Species) %>% 
    var_group(~ Sepal.Width + Petal.Width) %>%
    .expect_vars(z,character(),x)
  
  # Inferring missing y
  iris %>% dplyr::group_by(Species) %>% 
    var_group(~ Sepal.Width + Petal.Width, .infer_y = TRUE) %>%
    .expect_vars(z,y,x)
  
  # Multiple formulae
  iris %>% dplyr::group_by(Species) %>% 
    var_group(~ Sepal.Width, ~ Petal.Width, .infer_y = TRUE) %>%
    .expect_vars(z,y,x)
  
  # Multiple formulae 
  iris %>% dplyr::group_by(Species) %>% 
    var_group(Sepal.Length + Petal.Length ~  Sepal.Width, ~ Petal.Width, .infer_y = TRUE) %>%
    .expect_vars(z,y,x)
  
})


test_that("symbols interface works", {
  z = "Species"
  y = c("Sepal.Length","Petal.Length")
  x = c("Sepal.Width","Petal.Width")
  
  # Fully specified
  iris %>% dplyr::group_by(Species) %>% 
    var_group(Sepal.Width,Petal.Width) %>%
    .expect_vars(z,character(),x)
  
  # Lists interpreted as one
  iris %>% dplyr::group_by(Species) %>% 
    var_group(c(Sepal.Width,Petal.Width)) %>%
    .expect_vars(z,character(),x)
  
  # can splice in symbols
  tmp = lapply(x,as.symbol)
  iris %>% dplyr::group_by(Species) %>% 
    var_group(!!!tmp) %>%
    .expect_vars(z,character(),x)
  
})

test_that("vars interface works", {
  z = "Species"
  y = c("Sepal.Length","Petal.Length")
  x = c("Sepal.Width","Petal.Width")
  
  # Fully specified
  iris %>% dplyr::group_by(Species) %>% 
    var_group(dplyr::vars(Sepal.Width,Petal.Width)) %>%
    .expect_vars(z,character(),x)
  
  # Unsupported mix: 
  expect_error(
    iris %>% dplyr::group_by(Species) %>% 
      var_group(Sepal.Length + Petal.Length ~ Sepal.Width, dplyr::vars(Sepal.Width,Petal.Width)),
    regexp = ".*not correctly specified.*"
  )
  
})

test_that("character interface works", {
  z = "Species"
  y = c("Sepal.Length","Petal.Length")
  x = c("Sepal.Width","Petal.Width")
  
  iris %>% dplyr::group_by(Species) %>% 
    var_group(c(x,y)) %>%
    .expect_vars(z,character(),c(x,y))
  
})


test_that("tidyselect interface works", {
  z = "Species"
  y = c("Sepal.Length","Petal.Length")
  x = c("Sepal.Width","Petal.Width")
  
  # 2 parameter plus group
  iris %>% dplyr::group_by(Species) %>% 
    var_group(tidyselect::ends_with("Length"), tidyselect::ends_with("Width")) %>%
    .expect_vars(z,y,x)
  
  # 3 parameter
  iris %>% 
    var_group(c(Species), tidyselect::ends_with("Length"), tidyselect::ends_with("Width")) %>%
    .expect_vars(z,y,x)
  
  # 2 parameter
  iris %>% 
    var_group(tidyselect::ends_with("Length"), tidyselect::ends_with("Width")) %>%
    .expect_vars(character(),y,x)
  
  # 1 parameter with negation
  iris %>% 
    var_group(-c(Species,tidyselect::ends_with("Length"))) %>%
    .expect_vars(character(),character(),x)
  
  # common_error
  expect_error(
    iris %>% 
      var_group(Species,tidyselect::ends_with("Width")),
    regexp = ".*not correctly specified.*"
  )
  
})
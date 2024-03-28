
test_that("non dataframe works", {
  
  f = function(x,y,z) {
    resolve_missing(x = y + z, z = x-y, y = x-z)
    return(list(x=x,y=y,z=z))
  }
  y=1:5
  z=6:10
  tmp = f(y=y,z=z)
  expect_equal(tmp$x, y+z)
  
})


test_that("dataframe works", {
  
  f = function(x = NULL,y = NULL,z = NULL) {
    tmp = tibble::tibble(x=x,y=y,z=z)
    tmp = resolve_missing(x = y + z, z = x-y, y = x-z, .env = tmp, .eval_null = TRUE)
    return(tmp)
  }
  y=1:5
  z=6:10
  tmp = f(y=y,z=z)
  expect_equal(tmp$x, y+z)
  
})


test_that("integer coercion, non df", {
  
  a = 1:4
  b = 5:10
  check_integer(a,b)
  expect_true(is.integer(a))
  expect_true(is.integer(b))
  
})

test_that("integer coercion, df", {
  
  tmp = tibble::tibble(
    a = 1:4,
    b = 5:8
  )
  tmp = check_integer(a,b,.env = tmp)
  expect_true(is.integer(tmp$a))
  expect_true(is.integer(tmp$b))
  
})

test_that("numeric coercion, non df", {
  
  a = 1L:4L
  b = c("1.1","2.2","3.3","4.4")
  check_numeric(a,b)
  expect_true(is.numeric(a))
  expect_true(is.numeric(b))
  
})

test_that("integer coercion, df", {
  
  tmp = tibble::tibble(
    a = 1L:4L,
    b = c("1.1","2.2","3.3","4.4")
  )
  tmp = check_numeric(a,b,.env = tmp)
  expect_true(is.numeric(tmp$a))
  expect_true(is.numeric(tmp$b))
  
})


test_that("date coercion, non df", {
  
  a = c("1970-01-01","1970-01-02","1970-01-03","1970-01-04")
  b = Sys.Date() + 1:7
  check_date(a,b)
  expect_true(lubridate::is.Date(a))
  expect_true(lubridate::is.Date(b))
  
  d = c("01/01/1970","02/01/1970")
  check_date(d, format = "%d/%m/%Y")
  expect_true(lubridate::is.Date(d))
  
})
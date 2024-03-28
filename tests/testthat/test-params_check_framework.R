
test_fn = function(a) {
  check_numeric(a)
  return(a)
}

test_that("numeric coercion", {
  a = c(1,2,3)
  check_numeric(a)
  expect_equal(a, c(1,2,3))
})

test_that("integer to numeric coercion", {
  a = c(1L,2L,3L)
  check_numeric(a)
  expect_equal(a, c(1,2,3))
})

test_that("string to numeric coercion", {
  a = c("1","2","3")
  check_numeric(a)
  expect_equal(a, c(1,2,3))
})

test_that("factor to numeric coercion", {
  a = factor(c("A","B","C"))
  check_numeric(a)
  expect_equal(a, c(1,2,3))
})

test_that("numeric to integer coercion", {
  a = c(1.0,2.0,3.0)
  check_integer(a)
  expect_equal(a, c(1L,2L,3L))
})

test_that("string to integer coercion", {
  a = c("1.0","2.0","3.0")
  check_integer(a)
  expect_equal(a, c(1L,2L,3L))
})

test_that("integer coercion", {
  a = c("1.0","2.1","3.0")
  expect_error(check_integer(a),"rounding detected")
})

test_that("numeric coercion failure", {
  a = c("1.0","2.1","A3.0")
  expect_error(check_numeric(a),"non numeric format")
})

test_that("numeric coercion with NAs", {
  a = c("1.0","2.1",NA)
  check_numeric(a)
  expect_equal(a, c(1,2.1,NA))
})


test_that("date coercion", {
  a = c("2021-01-01","2021-01-02","2021-01-03")
  check_date(a)
  expect_equal(a, as.Date(c("2021-01-01","2021-01-02","2021-01-03")))
})

test_that("date coercion with format", {
  a = c("01/01/2021","02/01/2021","03/01/2021")
  check_date(a, format="%d/%m/%Y", origin=as.Date("2020-12-31"))
  expect_equal(a, as.Date(c("2021-01-01","2021-01-02","2021-01-03")))
})

test_that("date coercion from numeric", {
  a = 1:3
  check_date(a, format="%d/%m/%Y", origin=as.Date("2020-12-31"))
  expect_equal(a, as.Date(c("2021-01-01","2021-01-02","2021-01-03")))
})


test_fn = function(a, b, c=1, A=a, B=rep(b,length(a)), C=rep(c,length(a))) {
  force(A)
  force(B)
  force(C)
  i = recycle(a,b,c)
  if (!(rlang::is_missing(a) && rlang::is_missing(A))) expect_equal(a,A)
  if (!(rlang::is_missing(b) && rlang::is_missing(B))) expect_equal(b,B)
  if (!(rlang::is_missing(c) && rlang::is_missing(C))) expect_equal(c,C)
}

test_error = function(a, b=1:10) {
  recycle(a,b)
}

test_that("No-op works", {
  test_fn(a=1,b=2,c=3)
})

test_that("Recycling works", {
  test_fn(a=1:10, b=2, c=3)
})

test_that("Default works", {
  test_fn(a=1:10, b=2)
})

test_that("Incompatible error", {
  expect_error(test_error(a=1:2),regexp = "is/are the wrong lengths")
})

test_that("All NULL OK", {
  test_fn(NULL,NULL,NULL)
})

test_that("lists work", {
  test_fn(a=as.list(1:10), b=2)
})

test_that("Incompatible list error", {
  expect_error(test_error(a=list()),regexp = "is/are the wrong lengths")
})

test_that("Missing values ignored", {
  test_fn(a=1:10, B=rlang::missing_arg())
})

test_that("One NULL retained, but recycling works", {
  test_fn(a=1:10, b=NULL)
})

test_that("Empty list and NULLS works", {
  test_fn(a=list(), b=NULL, c=NULL)
})

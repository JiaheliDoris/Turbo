context("Test if regsubsets.fitted.values returns a vector with correct length")


test_that("regsubsets.fitted.values works", {
  set.seed(561)
  test_x <- rnorm(100)
  test_y <- cos(test_x)+rnorm(100,1,3)
  n <- 10
  fit <- regsubsets.fitted.values(test_x,test_y,n)
  expect_is(fit, "numeric")
  expect_length(fit, 100)
})

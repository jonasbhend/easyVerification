library(veriWrapper)
context('Conversion to probabilities')
xxx <- array(1:12, c(1,3,4))
xx <- array(1:12, c(3,4))
x <- 1:12

test_that("input argument is matrix or vector and non-missing", {
  expect_error(convert2prob(xxx), 'Input argument not of type vector or matrix')
  expect_error(convert2prob(as.factor(x)), 'Input argument not of type vector or matrix')
  expect_error(convert2prob(as.data.frame(xx)), 'Input argument not of type vector or matrix')
  expect_error(convert2prob(NA), 'No non-missing values in input')
})

test_that("argument is returned without modification", {
  expect_equal(convert2prob(x), x)
  expect_equal(convert2prob(xx), xx)
})

test_that("absolute threshold and probability are exchangeable", {
  expect_equal(convert2prob(xx, prob=0.5), convert2prob(xx, threshold=median(xx)))
  expect_equal(convert2prob(x, prob=0.5), convert2prob(x, threshold=median(x)))
  expect_equal(convert2prob(xx, prob=c(1/3, 2/3)), convert2prob(xx, threshold=quantile(xx, c(1/3,2/3))))
  expect_equal(convert2prob(x, prob=c(1/3,2/3)), convert2prob(x, threshold=quantile(x, c(1/3,2/3))))
})

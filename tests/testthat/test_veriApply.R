library(veriWrapper)
context('Wrapping')

xxx <- array(1, c(1,3,4))
xx <- array(1, c(3,4))
xxna <- xx
xxna[,2] <- NA
x <- rep(1,3)
xx2 <- array(1, c(2,3,4))
x2 <- array(1, 2:3)
x2na <- x2
x2na[2,] <- NA
xx3 <- array(1, c(3,5,6,3,4))

test_that("Error handling", {
  expect_error(veriApply(x))
  expect_error(veriApply('EnsMe', x, x))
  expect_error(veriApply('EnsMe', xx, x, tdim=3))
  expect_error(veriApply('EnsMe', xx, x, ensdim=3))
})

test_that('Output format and mode are correct', {
  expect_is(veriApply('EnsMe', xx, x), 'numeric')
  expect_is(veriApply('EnsCrps', xx, x), mode(EnsCrps(xx, x)))
  expect_equal(veriApply('EnsCrps', xx, x, tdim=1, ensdim=2), EnsCrps(xx, x))
  expect_equal(veriApply('EnsMe', xx3, xx3[,,,,1]), array(0, dim(xx3[,,,1,1])))
  expect_equal(veriApply('EnsMe', xx3, xx3[,,,1,], ensdim=4, tdim=5), array(0, dim(xx3[,,,1,1])))
  expect_equal(veriApply('EnsMe', xx3, xx3[,1,,,], ensdim=2, tdim=1), array(0, dim(xx3[1,1,,,])))
  expect_equal(veriApply('EnsMe', xx3, xx3[,1,,,], ensdim=2, tdim=4), array(0, dim(xx3[,1,,1,])))
})

test_that('Missing value handling', {
  expect_error(veriApply('EnsMe', xx, rep(NA, nrow(xx))))
  expect_error(veriApply('EnsMe', array(NA, dim(xx)), x))  
  expect_error(veriApply('EnsMe', xx, c(1,1,NA), na.rm=FALSE))
  expect_is(veriApply('EnsMe', xx, c(1,1,NA), na.rm=TRUE), 'numeric')
  expect_error(veriApply('EnsMe', xxna, x, na.rm=FALSE)) ## incomplete ensembles are never used
  expect_error(veriApply('EnsMe', xxna, x, na.rm=TRUE)) ## incomplete ensembles are never used
  expect_equal(veriApply('EnsMe', xx2, x2), array(0, 2))
  expect_equal(veriApply('EnsMe', xx2, x2na), array(c(0, NA), 2))
})
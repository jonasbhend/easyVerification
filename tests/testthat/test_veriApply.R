library(easyVerification)
context("Wrapping")

xxx <- array(1, c(1, 3, 4))
xx <- array(1, c(3, 4))
xxna <- xx
xxna[, 2] <- NA
xna <- x <- rep(1, 3)
xna[1] <- NA
xx2 <- array(1, c(2, 3, 4))
x2 <- array(1, 2:3)
x2na <- x2
x2na[2, ] <- NA
xx3 <- array(1, c(3, 5, 6, 3, 4))

test_that("Error handling", {
  expect_error(veriApply(x))
  expect_error(veriApply("EnsMe", x, x))
  expect_error(veriApply("EnsMe", xx, x, tdim = 3))
  expect_error(veriApply("EnsMe", xx, x, ensdim = 3))
})

test_that("Output format and mode are correct", {
  expect_is(veriApply("EnsMe", xx, x), "numeric")
  expect_is(veriApply("EnsCrps", xx, x), mode(EnsCrps(xx, x)))
  expect_equal(veriApply("EnsCrps", xx, x, tdim = 1, ensdim = 2), EnsCrps(xx, x))
  expect_equal(veriApply("EnsMe", fcst = xx3, xx3[, , , , 1]), array(0, dim(xx3[, , , 1, 1])))
  expect_equal(veriApply("EnsMe", fcst = xx3, xx3[, , , 1, ], ensdim = 4, tdim = 5), array(0, dim(xx3[, , , 1, 1])))
  expect_equal(veriApply("EnsMe", fcst = xx3, xx3[, 1, , , ], ensdim = 2, tdim = 1), array(0, dim(xx3[1, 1, , , ])))
  expect_equal(veriApply("EnsMe", fcst = xx3, xx3[, 1, , , ], ensdim = 2, tdim = 4), array(0, dim(xx3[, 1, , 1, ])))
  expect_equal(veriApply("EnsCrps", fcst = xx3, xx3[, , , , 1]), array(0, dim(xx3[, , , , 1])))
  expect_equal(veriApply("EnsCrps", fcst = xx3, xx3[, , , 1, ], ensdim = 4, tdim = 5), array(0, dim(xx3[, , , 1, ])))
  expect_equal(veriApply("EnsCrps", fcst = xx3, xx3[, 1, , , ], ensdim = 2, tdim = 1), array(0, dim(xx3[, 1, , , ])))
  expect_equal(veriApply("EnsCrps", fcst = xx3, xx3[, 1, , , ], ensdim = 2, tdim = 4), array(0, dim(xx3[, 1, , , ])))
  expect_equal(veriApply("EnsCrps", fcst = xx3, xx3[1, , , , ], ensdim = 1, tdim = 3), array(0, dim(xx3[1, , , , ])))
})

test_that("Missing value handling", {
  expect_warning(veriApply("EnsMe", xx, rep(NA, nrow(xx))))
  expect_true(is.na(veriApply("EnsMe", xx, rep(NA, nrow(xx)))))
  expect_warning(veriApply("EnsMe", array(NA, dim(xx)), x))
  expect_true(is.na(veriApply("EnsMe", array(NA, dim(xx)), x)))
  expect_warning(veriApply("EnsMe", xx, c(1, 1, NA), na.rm = FALSE))
  expect_true(is.na(veriApply("EnsMe", xx, c(1, 1, NA), na.rm = FALSE)))
  expect_is(veriApply("EnsMe", xx, c(1, 1, NA), na.rm = TRUE), "numeric")
  expect_warning(veriApply("EnsMe", xxna, x, na.rm = FALSE)) ## incomplete ensembles are never used
  expect_true(is.na(veriApply("EnsMe", xxna, x, na.rm = FALSE)))
  expect_warning(veriApply("EnsMe", xxna, x, na.rm = TRUE)) ## incomplete ensembles are never used
  expect_true(is.na(veriApply("EnsMe", xxna, x, na.rm = TRUE)))
  expect_equal(veriApply("EnsMe", xx2, x2), array(0, 2))
  expect_equal(veriApply("EnsMe", xx2, x2na), array(c(0, NA), 2))
  expect_is(veriApply("EnsRps", xx, xna, prob = 1:2 / 3, na.rm = T), "numeric")
  expect_warning(veriApply("EnsRps", xx, xna, prob = 1:2 / 3))
  expect_true(all(is.na(veriApply("EnsRps", xx, xna, prob = 1:2 / 3))) &
    length(veriApply("EnsRps", xx, xna, prob = 1:2 / 3)) == nrow(xx))
  #  expect_equivalent(veriApply("EnsRps", xx2, rbind(xna, x), prob=1:2/3),
  #               rbind(NA, veriApply("EnsRps", xx2[2,,], x, prob=1:2/3)))
  tm <- toymodel()
  tm$obs[3:5] <- NA
  expect_true(is.na(suppressWarnings(veriApply("FairCrpss", tm$fcst, tm$obs)[[1]])))
  expect_true(!is.na(veriApply("FairCrpss", tm$fcst, tm$obs, na.rm = T)[[1]]))
  expect_true(is.na(suppressWarnings(veriApply("FairCrpss", tm$fcst, tm$obs, na.rm = T, nmin = 33)[[1]])))
  expect_true(is.na(suppressWarnings(veriApply("FairCrpss", tm$fcst, tm$obs, na.rm = T, fracmin = 0.95)[[1]])))
  expect_true(is.na(suppressWarnings(veriApply("FairRpss", tm$fcst, tm$obs, threshold = 0)[[1]])))
  expect_true(!is.na(veriApply("FairRpss", tm$fcst, tm$obs, threshold = 0, na.rm = T)[[1]]))
  expect_true(is.na(suppressWarnings(veriApply("FairRpss", tm$fcst, tm$obs, threshold = 0, na.rm = T, nmin = 33)[[1]])))
  expect_true(is.na(suppressWarnings(veriApply("FairRpss", tm$fcst, tm$obs, threshold = 0, na.rm = T, fracmin = 0.95)[[1]])))
})

test_that("Reference forecasts for skill scores", {
  x <- rep(1, 3)
  xx <- array(rnorm(15, mean = 1), c(3, 5))
  expect_true(veriApply("EnsCrpss", xx, x)[[1]] < veriApply("EnsCrpss", xx, x, fcst.ref = 0 * xx)[[1]])
  expect_equal(veriApply("EnsCrpss", xx, x)[[1]], veriApply("EnsCrpss", xx, x, fcst.ref = t(array(x, rep(length(x), 2))))[[1]])
})

test_that("Multidimensional probability and absolute thresholds", {
  fcst <- array(rnorm(5 * 3 * 10 * 5), c(5, 3, 10, 5))
  obs <- array(rnorm(5 * 3 * 10), c(5, 3, 10))
  signal <- runif(5 * 3, min = 3, max = 9)
  prob <- array(rep(1:2 / 3, each = 5 * 3), c(5, 3, 2))
  expect_equal(
    veriApply("EnsRpss", fcst = fcst, obs = obs, prob = 1:2 / 3),
    veriApply("EnsRpss", fcst = fcst, obs = obs, prob = prob)
  )
  expect_equal(
    veriApply("EnsRpss", fcst = fcst, obs = obs, prob = 1:2 / 3),
    veriApply("EnsRpss", fcst = fcst, obs = obs, prob = matrix(prob, 5 * 3, 2))
  )
  expect_equal(
    veriApply("EnsRpss",
      fcst = aperm(fcst, c(1, 3, 2, 4)),
      obs = aperm(obs, c(1, 3, 2)), prob = 1:2 / 3, tdim = 2
    ),
    veriApply("EnsRpss",
      fcst = aperm(fcst, c(1, 3, 2, 4)),
      obs = aperm(obs, c(1, 3, 2)), prob = aperm(prob, c(1, 3, 2)), tdim = 2
    )
  )
  expect_equal(
    veriApply("EnsRpss", fcst = fcst, obs = obs, threshold = 1),
    veriApply("EnsRpss", fcst = fcst + signal, obs = obs + signal, threshold = as.matrix(signal) + 1)
  )
})

test_that("Reference forecasts", {
  fcst <- array(rnorm(5 * 3 * 10 * 5), c(5, 3, 10, 5))
  obs <- array(rnorm(5 * 3 * 10), c(5, 3, 10))
  ro.cross <- lapply(1:10, function(x) setdiff(1:10, x))
  ro.forward <- lapply(1:10, function(x) seq(if (x > 5) 1 else x + 1, if (x > 5) x - 1 else 10))
  ro.block <- lapply(1:10, function(x) setdiff(1:10, x + seq(-1, 1)))
  sub.ind <- setdiff(1:10, c(1, 5, 9:10))
  ro.sub <- lapply(1:10, function(x) sub.ind)
  ro.subcross <- lapply(1:10, function(x) setdiff(sub.ind, x))
  expect_equal(
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = "forward"),
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = ro.forward)
  )
  expect_equal(
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = "crossval"),
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = ro.cross)
  )
  expect_equal(
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = list(type = "crossval", blocklength = 3)),
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = ro.block)
  )
  expect_equal(
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = list(indices = sub.ind)),
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = ro.sub)
  )
  expect_equal(
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = list(type = "crossval", indices = sub.ind)),
    veriApply("FairCrpss", fcst = fcst, obs = obs, strategy = ro.subcross)
  )
  expect_equal(
    veriApply("FairRpss", fcst = fcst, obs = obs, prob = 1:2 / 3),
    veriApply("climFairRpss", fcst = fcst, obs = obs, prob = 1:2 / 3)
  )
  expect_message(veriApply("FairRpss", fcst = fcst, obs = obs, prob = 1:2 / 3))
  expect_false(all(unlist(veriApply("FairRpss", fcst = fcst, obs = obs, threshold = 0)) == unlist(veriApply("climFairRpss", fcst = fcst, obs = obs, threshold = 0))))
})

# Every rejected input must name the offending argument, and where a working
# alternative exists, name that too.

test_that("scalar arguments are validated", {
  v <- rnorm(200); s <- rep(1:20, each = 10)
  expect_error(segmented_qcc(v, "xbar", s, nsigma = 0),        "`nsigma`")
  expect_error(segmented_qcc(v, "xbar", s, nsigma = -3),       "`nsigma`")
  expect_error(segmented_qcc(v, "xbar", s, nsigma = c(2, 3)),  "`nsigma`")
  expect_error(segmented_qcc(v, "xbar", s, nsigma = NA),       "`nsigma`")
  expect_error(segmented_qcc(v, "xbar", s, min_seg_len = 0),   "`min_seg_len`")
  expect_error(segmented_qcc(v, "xbar", s, min_seg_len = 2.5), "whole number")
  expect_error(segmented_qcc(v, "xbar", s, plot = NA),         "`plot`")
  expect_error(segmented_qcc(v, "xbar", s, method = "NOPE"),   "`method`")
  expect_error(segmented_qcc(v, "xbar", s, penalty = "NOPE"),  "`penalty`")
  expect_error(segmented_qcc(v, "xbar", s, cpt_stat = "NOPE"), "`cpt_stat`")
})

test_that("bad method and penalty messages list the valid values", {
  v <- rnorm(200); s <- rep(1:20, each = 10)
  expect_error(segmented_qcc(v, "xbar", s, method = "NOPE"),  "PELT")
  expect_error(segmented_qcc(v, "xbar", s, penalty = "NOPE"), "MBIC")
})

test_that("value itself is validated", {
  expect_error(segmented_qcc(numeric(0), type = "c"), "empty")
  expect_error(segmented_qcc(letters, type = "c"), "must be numeric")
  expect_error(segmented_qcc(c(1, NA, 3), type = "c"), "NA")
  expect_error(segmented_qcc(c(1, Inf, 3), type = "c"), "Inf")
  expect_error(segmented_qcc(c(1, -2, 3), type = "c"), "negative")
})

test_that("variable charts require a usable sample argument", {
  expect_error(segmented_qcc(rnorm(100), type = "xbar"), "`sample` is required")
  expect_error(segmented_qcc(1:10, type = "xbar", sample = 1:9), "same length")
  expect_error(segmented_qcc(c(1, NA), type = "xbar", sample = c(1, 2)), "NA")
  expect_error(segmented_xbar(1:10, 1:9), "same length")
})

test_that("p and np require sizes consistent with value", {
  v <- rbinom(100, 50, 0.1)
  expect_error(segmented_qcc(v, type = "p"), "`sizes` is required")
  expect_error(segmented_qcc(v, type = "p", sizes = rep(50, 99)), "length 99")
  expect_error(segmented_qcc(v, type = "p", sizes = rep(0, 100)), "positive")
  expect_error(segmented_qcc(rep(60, 100), type = "p", sizes = rep(50, 100)),
               "cannot exceed")
  expect_error(segmented_qcc(v, type = "p", sizes = rep(50, 100),
                             area = rep(1, 100)), "`area` applies only")
})

test_that("u requires area and c refuses both", {
  v <- rpois(100, 3)
  expect_error(segmented_qcc(v, type = "u"), "`area` is required")
  expect_error(segmented_qcc(v, type = "u", area = rep(10, 99)), "length 99")
  expect_error(segmented_qcc(v, type = "u", sizes = rep(10, 100)), "use `area`")
  expect_error(segmented_qcc(v, type = "c", sizes = 5), 'type = "c"')
  expect_error(segmented_qcc(v, type = "c", area = rep(1, 100)), "u chart")
})

test_that("xbar.one refuses sizes and area", {
  expect_error(segmented_qcc(rnorm(50), type = "xbar.one", sizes = rep(1, 50)),
               "do not apply")
})

test_that("sample is ignored, with a warning, when it cannot apply", {
  expect_warning(segmented_qcc(rpois(100, 3), type = "c", sample = 1:100),
                 "is ignored")
})

test_that("non-integer counts warn but are honoured", {
  set.seed(1)
  expect_warning(fit <- segmented_qcc(runif(100, 0, 5), type = "c"),
                 "non-integer")
  expect_equal(fit$n_samples, 100)
})

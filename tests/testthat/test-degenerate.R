# The package is meant to be pointed at arbitrary series: anything that stops
# change-point detection must degrade to an ordinary single-segment chart with a
# warning, never abort.

expect_single_segment <- function(fit) {
  expect_equal(nrow(fit$segments), 1L)
  expect_length(fit$change.points, 0)
  expect_false(fit$segmented)
  expect_gt(length(fit$notes), 0)
}

test_that("a series too short to split still returns a chart", {
  expect_warning(fit <- segmented_qcc(rnorm(5, 100, 2), type = "xbar",
                                      sample = rep(1, 5)), "1 sample")
  expect_single_segment(fit)
})

test_that("min_seg_len larger than the series degrades gracefully", {
  set.seed(4); v <- rnorm(500, 100, 2); s <- rep(1:100, each = 5)
  expect_warning(fit <- segmented_qcc(v, "xbar", s, min_seg_len = 80),
                 "min_seg_len = 80")
  expect_single_segment(fit)
  expect_warning(fit2 <- segmented_qcc(v, "xbar", s, min_seg_len = 200),
                 "min_seg_len = 200")
  expect_single_segment(fit2)
})

test_that("a constant statistic is reported, not split", {
  expect_warning(fit <- segmented_qcc(rep(100, 500), type = "xbar",
                                      sample = rep(1:100, each = 5)),
                 "constant")
  expect_equal(nrow(fit$segments), 1L)
  expect_length(fit$change.points, 0)
})

test_that("a detector that refuses the data degrades to one segment", {
  # BinSeg needs more points than this series has.
  set.seed(1)
  expect_warning(fit <- segmented_qcc(rnorm(15, 100, 2), type = "xbar",
                                      sample = rep(1:3, each = 5),
                                      method = "BinSeg"),
                 "change-point detection failed")
  expect_single_segment(fit)
  expect_true(all(is.finite(c(fit$segments$LCL, fit$segments$UCL))))
})

test_that("a segment qcc cannot refit falls back to the global limits", {
  # An individuals chart needs two samples for a moving range, so a one-sample
  # segment has no limits of its own.
  set.seed(7)
  v <- c(rnorm(30, 50, 1), 80, rnorm(30, 50, 1))
  expect_warning(fit <- segmented_qcc(v, type = "xbar.one"),
                 "used the limits of the whole series")
  expect_true("global" %in% fit$segments$limits_from)
  expect_true(all(is.finite(c(fit$segments$LCL, fit$segments$UCL))))
})

test_that("a single sample yields a chart without limits, not an error", {
  expect_warning(fit <- segmented_qcc(42, type = "xbar.one"))
  expect_equal(fit$n_samples, 1L)
  expect_equal(nrow(fit$segments), 1L)
})

test_that("a three-sample attribute series works", {
  expect_silent(fit <- segmented_qcc(c(3, 4, 5), type = "c"))
  expect_equal(fit$n_samples, 3L)
})

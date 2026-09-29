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

test_that("a short attribute series returns a chart, with the reason", {
  # A c chart uses the Poisson cost, which needs segments of at least two
  # samples, so three samples cannot be split at all.
  expect_warning(fit <- segmented_qcc(c(3, 4, 5), type = "c"),
                 "needs at least 4 for a split")
  expect_equal(fit$n_samples, 3L)
  expect_equal(nrow(fit$segments), 1L)
  expect_silent(segmented_qcc(c(3, 4, 5, 6), type = "c"))
})

test_that("a segment too short to flag anything is reported", {
  # A segment fitted around a single sample puts its limits either side of that
  # sample, so the sample stops being out of control: the split hides the very
  # signal it was reacting to.
  set.seed(51)
  v <- c(rnorm(1200, 10, 1), rnorm(40, 10, 6), rnorm(1200, 10, 1))
  s <- rep(1:61, each = 40)
  expect_warning(fit <- segmented_qcc(v, type = "S", sample = s),
                 "nothing in them can come out as out of control")
  short <- fit$segments[fit$segments$n_samples < 3, ]
  expect_gt(nrow(short), 0)
  expect_true(all(short$n_out == 0))
})

test_that("raising min_seg_len turns those segments into out-of-control points", {
  set.seed(51)
  v <- c(rnorm(1200, 10, 1), rnorm(40, 10, 6), rnorm(1200, 10, 1))
  s <- rep(1:61, each = 40)
  loose <- suppressWarnings(segmented_qcc(v, type = "S", sample = s))
  tight <- segmented_qcc(v, type = "S", sample = s, min_seg_len = 10)
  odd <- 31L                                   # the inflated sample
  expect_false(odd %in% loose$out_of_control)  # hidden by its own segment
  expect_true(odd %in% tight$out_of_control)   # flagged, as it should be
  expect_length(tight$notes, 0)
})

test_that("a single-segment series is not reported as short", {
  # One segment covering a 2-sample series is the whole series, not a split
  # isolating an outlier; warning about it would be noise.
  fit <- suppressWarnings(segmented_qcc(c(3, 5), type = "c"))
  expect_equal(nrow(fit$segments), 1L)
  expect_false(any(grepl("come out as out of control", fit$notes)))
})

test_that("many short segments are reported in one warning, not hundreds", {
  # An over-segmented series can produce hundreds of one-sample segments; a
  # warning each would bury every other message.
  set.seed(24)
  w <- testthat::capture_warnings(
    segmented_qcc(rnorm(4000, 100, 20), type = "xbar",
                  sample = rep(1:400, each = 10), scale = "none"))
  short <- grep("fewer than 3 samples", w, value = TRUE)
  expect_length(short, 1)
  expect_match(short, "more\\)")          # the list is truncated
})

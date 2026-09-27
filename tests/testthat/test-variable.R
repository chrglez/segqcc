# Variable charts: xbar, R, S, and the individuals chart. Bundled data only.

test_that("xbar recovers the two embedded change points", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_s3_class(fit, "segmented_qcc")
  expect_length(fit$change.points, 2)          # true shifts at 120 and 240
  expect_true(fit$change.points[1] %in% 115:125)
  expect_true(fit$change.points[2] %in% 235:245)
  expect_equal(nrow(fit$segments), 3)
  expect_true(fit$segmented)
  expect_length(fit$notes, 0)
})

test_that("xbar segment centres track the true segment means", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_equal(fit$segments$center, c(100, 101.5, 99.5), tolerance = 0.3)
})

test_that("segment limits are finite and ordered", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  s <- fit$segments
  expect_true(all(is.finite(c(s$LCL, s$UCL, s$center))))
  expect_true(all(s$LCL < s$center & s$center < s$UCL))
  expect_true(all(s$limits_from == "segment"))
})

test_that("segments tile the series exactly once", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  s <- fit$segments
  expect_equal(s$from[1], 1L)
  expect_equal(s$to[nrow(s)], max(segxbar$sample))
  expect_equal(s$from[-1], s$to[-nrow(s)] + 1L)
  expect_equal(sum(s$n_samples), max(segxbar$sample))
})

test_that("out_of_control is consistent with the per-segment limits", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  st  <- as.numeric(fit$statistics)
  s   <- fit$segments
  LCL <- rep(s$LCL, s$n_samples); UCL <- rep(s$UCL, s$n_samples)
  expect_equal(fit$out_of_control, unname(which(st < LCL | st > UCL)))
  expect_equal(sum(s$n_out), length(fit$out_of_control))
})

test_that("S chart runs on large samples and R refuses them", {
  fit <- segmented_qcc(segxbar$value, type = "S", sample = segxbar$sample)
  expect_true(all(is.finite(c(fit$segments$LCL, fit$segments$UCL))))
  expect_true(all(fit$segments$LCL < fit$segments$UCL))

  # qcc tabulates the R-chart constants only to n = 25, so a larger sample must
  # be rejected with a pointer to the S chart, not produce NA limits.
  big <- rep(1:20, each = 30)
  expect_error(segmented_qcc(rnorm(600), type = "R", sample = big), 'type = "S"')
})

test_that("R chart works within its tabulated sample-size range", {
  fit <- segmented_qcc(segxbar$value[1:1600], type = "R",
                       sample = segxbar$sample[1:1600])
  expect_true(all(is.finite(c(fit$segments$LCL, fit$segments$UCL))))
})

test_that("segmented_xbar is a shorthand for segmented_qcc", {
  f1 <- segmented_xbar(segxbar$value, segxbar$sample)
  f2 <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_equal(f1$change.points, f2$change.points)
  expect_equal(f1$segments, f2$segments)
})

test_that("individuals chart handles samples of size 1", {
  fit <- segmented_qcc(segind$value, type = "xbar.one")
  expect_equal(fit$n_samples, nrow(segind))
  expect_length(fit$change.points, 2)          # true shifts at 150 and 250
  expect_true(fit$change.points[1] %in% 145:155)
  expect_true(fit$change.points[2] %in% 245:255)
  expect_equal(fit$segments$center, c(50, 53, 51), tolerance = 0.4)
})

test_that("xbar on size-1 samples points at the individuals chart", {
  expect_error(segmented_qcc(rnorm(50), type = "xbar", sample = 1:50),
               'type = "xbar.one"')
})

test_that("a homogeneous process is not segmented", {
  set.seed(3)
  fit <- segmented_qcc(rnorm(8000, 50, 4), type = "xbar",
                       sample = rep(1:400, each = 20))
  expect_length(fit$change.points, 0)
  expect_equal(nrow(fit$segments), 1)
  expect_false(fit$segmented)
})

# Run rules. qcc's own rule is reused, but applied within each segment: a run
# that straddles a change point compares samples against two different centres.

test_that("runs are reported and counted per segment", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_true(is.integer(fit$violating_runs) || is.numeric(fit$violating_runs))
  expect_true(all(fit$violating_runs >= 1 & fit$violating_runs <= fit$n_samples))
  expect_equal(sum(fit$segments$n_runs), length(fit$violating_runs))
  # the object qcc consumers read is the segment-aware one
  expect_equal(fit$violations$violating.runs, fit$violating_runs)
})

test_that("a run is not allowed to straddle a change point", {
  # A long stretch above the centre that continues across a level shift: qcc
  # counting over the whole series would call the samples just after the shift
  # part of the run that began before it.
  set.seed(41)
  v <- rnorm(4000, rep(c(0, 4), each = 2000), 1)
  s <- rep(1:400, each = 10)
  fit <- segmented_qcc(v, type = "xbar", sample = s)
  expect_length(fit$change.points, 1)
  cp <- fit$change.points[1]
  # no reported run may contain samples from two different segments
  for (i in seq_len(nrow(fit$segments))) {
    inside <- fit$violating_runs[fit$violating_runs >= fit$segments$from[i] &
                                 fit$violating_runs <= fit$segments$to[i]]
    expect_true(all(inside > cp) || all(inside <= cp))
  }
})

test_that("run_length controls the rule and 0 switches it off", {
  fit0 <- segmented_qcc(segxbar$value, type = "xbar",
                        sample = segxbar$sample, run_length = 0)
  expect_length(fit0$violating_runs, 0)
  expect_true(all(fit0$segments$n_runs == 0))

  # a shorter required run can only flag at least as many samples
  short <- segmented_qcc(segxbar$value, type = "xbar",
                         sample = segxbar$sample, run_length = 3)
  long  <- segmented_qcc(segxbar$value, type = "xbar",
                         sample = segxbar$sample, run_length = 12)
  expect_gte(length(short$violating_runs), length(long$violating_runs))
})

test_that("run_length is validated", {
  v <- segxbar$value; s <- segxbar$sample
  expect_error(segmented_qcc(v, "xbar", s, run_length = -1), "`run_length`")
  expect_error(segmented_qcc(v, "xbar", s, run_length = 2.5), "whole number")
})

test_that("runs default to qcc's own run.length option", {
  fit <- segmented_qcc(segc$value, type = "c")
  expect_equal(fit$run_length, qcc::qcc.options("run.length"))
})

test_that("print and summary report runs", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_match(paste(capture.output(print(fit)), collapse = "\n"),
               "Violating runs")
  expect_true("n_runs" %in% names(fit$segments))
})

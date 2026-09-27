# Sample handling, print/summary/plot.

test_that("samples are ordered by first appearance, not by label", {
  # Labels sorted differently from their time order: qcc.groups() would sort
  # them and silently permute the series.
  set.seed(31)
  v <- rnorm(600, rep(c(100, 106), each = 300), 1)
  lab <- rep(sprintf("s%02d", c(30:1, 60:31)), each = 10)
  fit <- segmented_qcc(v, type = "xbar", sample = lab)
  expect_equal(fit$sample.labels, unique(lab))
  expect_length(fit$change.points, 1)
  expect_equal(fit$change.points[1], 30)
})

test_that("non-canonical sample labels give the same answer as 1..K", {
  set.seed(32)
  v <- rnorm(1000, rep(c(100, 104), each = 500), 1)
  ref <- segmented_qcc(v, "xbar", rep(1:100, each = 10))$change.points
  expect_equal(segmented_qcc(v, "xbar", rep(5:104, each = 10))$change.points, ref)
  expect_equal(segmented_qcc(v, "xbar", rep(seq(1, 199, 2), each = 10))$change.points, ref)
  expect_equal(segmented_qcc(v, "xbar", rep(sprintf("g%03d", 1:100), each = 10))$change.points, ref)
  expect_equal(segmented_qcc(v, "xbar", factor(rep(sprintf("g%03d", 1:100), each = 10)))$change.points, ref)
})

test_that("unequal sample sizes warn and still produce a chart", {
  set.seed(33)
  expect_warning(fit <- segmented_qcc(rnorm(95, 100, 2), type = "xbar",
                                      sample = rep(1:10, c(5, rep(10, 9)))),
                 "unequal sizes")
  expect_equal(fit$n_samples, 10L)
})

test_that("print and summary report the segments", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  out <- capture.output(print(fit))
  expect_match(paste(out, collapse = "\n"), "Segmented xbar control chart")
  expect_match(paste(out, collapse = "\n"), "Change points:")
  expect_equal(length(grep("^[0-9]+ ", out)), 3)     # one row per segment
  expect_match(paste(capture.output(summary(fit)), collapse = "\n"),
               "Standard deviation")
  capture.output(vis <- withVisible(print(fit)))
  expect_false(vis$visible)
  expect_identical(vis$value, fit)
})

test_that("plotting works for every chart type and refuses other objects", {
  pdf(NULL); on.exit(dev.off())
  expect_silent(plot(segmented_qcc(segxbar$value, "xbar", segxbar$sample)))
  expect_silent(plot(segmented_qcc(segind$value, type = "xbar.one")))
  expect_silent(plot(segmented_qcc(segc$value, type = "c")))
  expect_silent(plot(segmented_qcc(segp$value, type = "p", sizes = segp$sizes)))
  expect_error(plot_segmented_qcc(list(a = 1)), "segmented_qcc object")
})

test_that("plot = TRUE draws without altering the result", {
  pdf(NULL); on.exit(dev.off())
  f1 <- segmented_qcc(segc$value, type = "c")
  f2 <- segmented_qcc(segc$value, type = "c", plot = TRUE)
  expect_equal(f1$segments, f2$segments)
})

test_that("par settings are restored after plotting", {
  pdf(NULL); on.exit(dev.off())
  before <- par("mar")
  plot(segmented_qcc(segc$value, type = "c"))
  expect_equal(par("mar"), before)
})

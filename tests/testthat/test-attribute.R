# Attribute charts: p, np, c, u.

test_that("p chart finds the bundled defect-rate shift", {
  fit <- segmented_qcc(segp$value, type = "p", sizes = segp$sizes)
  expect_equal(fit$n_samples, nrow(segp))
  expect_length(fit$change.points, 1)          # true shift at 150
  expect_true(fit$change.points[1] %in% 140:160)
  expect_equal(nrow(fit$segments), 2)
  # 0.05 -> 0.09, so the second segment must sit higher
  expect_lt(fit$segments$center[1], fit$segments$center[2])
  expect_equal(fit$segments$center, c(0.05, 0.09), tolerance = 0.02)
})

test_that("np chart is the p chart on the count scale", {
  fp <- segmented_qcc(segp$value, type = "p",  sizes = segp$sizes)
  fn <- segmented_qcc(segp$value, type = "np", sizes = segp$sizes)
  expect_equal(fn$change.points, fp$change.points)
  expect_equal(fn$segments$center, fp$segments$center * 50, tolerance = 1e-6)
})

test_that("c chart finds the bundled Poisson shift without tuning", {
  fit <- segmented_qcc(segc$value, type = "c")
  expect_length(fit$change.points, 1)          # true shift at 150
  expect_true(fit$change.points[1] %in% 140:160)
  expect_equal(nrow(fit$segments), 2)
  expect_equal(fit$segments$center, c(3, 5), tolerance = 0.5)
})

test_that("u chart accounts for the inspection area", {
  set.seed(1)
  area <- rep(c(10, 20), length.out = 300)
  v <- rpois(300, area * c(rep(0.3, 150), rep(0.6, 150)))
  fit <- segmented_qcc(v, type = "u", area = area)
  expect_length(fit$change.points, 1)
  expect_true(fit$change.points[1] %in% 140:160)
  expect_equal(fit$segments$center, c(0.3, 0.6), tolerance = 0.08)
})

test_that("attribute limits never fall below zero", {
  fit <- segmented_qcc(segc$value, type = "c")
  expect_true(all(fit$segments$LCL >= 0))
})

# ---- varying sample sizes ---------------------------------------------------
# A p, np or u chart whose sample sizes vary has a different pair of limits for
# every sample, because the limit depends on n. Reusing one pair across a
# segment judges most of the series against a band that never applied to it.

make_p <- function() {
  set.seed(1)
  n <- sample(40:120, 200, replace = TRUE)
  list(n = n, v = rbinom(200, n, c(rep(0.05, 100), rep(0.10, 100))))
}

test_that("limits vary within a segment when the sample sizes do", {
  d <- make_p()
  fit <- segmented_qcc(d$v, type = "p", sizes = d$n)
  expect_true(all(fit$segments$limits_vary))
  expect_true(all(fit$segments$UCL_min < fit$segments$UCL))
  # the per-sample limits survive in the object rather than being flattened
  expect_gt(nrow(unique(fit$limits)), 1)
  expect_equal(nrow(fit$limits), fit$n_samples)
})

test_that("out-of-control matches a per-sample refit exactly", {
  d <- make_p()
  fit <- segmented_qcc(d$v, type = "p", sizes = d$n)
  expected <- integer(0)
  for (i in seq_len(nrow(fit$segments))) {
    idx <- fit$segments$from[i]:fit$segments$to[i]
    q <- qcc::qcc(d$v[idx], sizes = d$n[idx], type = "p", plot = FALSE)
    lim <- q$limits
    if (!is.matrix(lim)) lim <- matrix(lim, nrow = length(idx), ncol = 2,
                                       byrow = TRUE)
    st <- as.numeric(q$statistics)
    expected <- c(expected, idx[st < lim[, 1] | st > lim[, 2]])
  }
  expect_equal(as.integer(fit$out_of_control), as.integer(expected))
})

test_that("each sample is judged against its own limits", {
  d <- make_p()
  fit <- segmented_qcc(d$v, type = "p", sizes = d$n)
  st  <- as.numeric(fit$statistics)
  oc  <- which(st < fit$limits[, 1] | st > fit$limits[, 2])
  expect_equal(fit$out_of_control, unname(oc))
  # a larger sample gets a tighter band
  i <- which.max(d$n); j <- which.min(d$n)
  w <- fit$limits[, 2] - fit$limits[, 1]
  expect_lt(w[i], w[j])
})

test_that("a u chart with varying area also gets per-sample limits", {
  set.seed(2)
  area <- sample(c(5, 10, 20), 200, replace = TRUE)
  v <- rpois(200, area * c(rep(0.3, 100), rep(0.6, 100)))
  fit <- segmented_qcc(v, type = "u", area = area)
  expect_true(any(fit$segments$limits_vary))
  expect_gt(nrow(unique(fit$limits)), 1)
})

test_that("constant sample sizes leave the limits constant", {
  fit <- segmented_qcc(segp$value, type = "p", sizes = segp$sizes)
  expect_false(any(fit$segments$limits_vary))
  expect_equal(fit$segments$LCL, fit$segments$LCL_max)
  expect_equal(fit$segments$UCL_min, fit$segments$UCL)
  cc <- segmented_qcc(segc$value, type = "c")
  expect_false(any(cc$segments$limits_vary))
})

test_that("the range columns are hidden when nothing varies", {
  out <- capture.output(print(segmented_qcc(segc$value, type = "c")))
  expect_false(any(grepl("LCL_max", out)))
  d <- make_p()
  out2 <- capture.output(print(segmented_qcc(d$v, type = "p", sizes = d$n)))
  expect_true(any(grepl("LCL_max", out2)))
  expect_true(any(grepl("Limits vary within a segment", out2)))
})

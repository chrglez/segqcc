# changepoint's normal cost assumes a unit-variance series. Without
# standardisation the detector over-segments in proportion to the dispersion of
# the charted statistic, so scale invariance is a correctness property here.

test_that("detection is invariant to a change of units", {
  set.seed(21)
  base <- rnorm(4000, 0, 1) + rep(c(0, 3, 1), times = c(1600, 1200, 1200))
  s <- rep(1:400, each = 10)
  ref <- segmented_qcc(base, type = "xbar", sample = s)$change.points
  for (mult in c(0.01, 1, 100, 10000)) {
    got <- segmented_qcc(base * mult, type = "xbar", sample = s)$change.points
    expect_equal(got, ref, info = paste("multiplier", mult))
  }
})

test_that("detection is invariant to a shift of origin", {
  set.seed(22)
  base <- rnorm(4000, 0, 2) + rep(c(0, 4), each = 2000)
  s <- rep(1:400, each = 10)
  ref <- segmented_qcc(base, type = "xbar", sample = s)$change.points
  for (off in c(-1e4, 0, 1e4)) {
    got <- segmented_qcc(base + off, type = "xbar", sample = s)$change.points
    expect_equal(got, ref, info = paste("offset", off))
  }
})

test_that("a homogeneous series is not split at any dispersion", {
  for (sd in c(0.5, 2, 5, 20)) {
    set.seed(23)
    fit <- segmented_qcc(rnorm(4000, 100, sd), type = "xbar",
                         sample = rep(1:400, each = 10))
    expect_length(fit$change.points, 0)
  }
})

test_that("scale = \"none\" reproduces the unstandardised behaviour", {
  # Kept as an escape hatch; on a widely dispersed homogeneous series it is
  # exactly what over-segments, which is why it is not the default.
  set.seed(24)
  v <- rnorm(4000, 100, 20); s <- rep(1:400, each = 10)
  expect_length(segmented_qcc(v, "xbar", s, scale = "mr")$change.points, 0)
  expect_gt(length(suppressWarnings(
    segmented_qcc(v, "xbar", s, scale = "none"))$change.points), 0)
})

test_that("mr and sd scalings agree on a clean multi-shift series", {
  set.seed(25)
  v <- rnorm(4000, rep(c(100, 103, 100, 106), each = 1000), 2)
  s <- rep(1:400, each = 10)
  expect_equal(segmented_qcc(v, "xbar", s, scale = "mr")$change.points,
               segmented_qcc(v, "xbar", s, scale = "sd")$change.points)
})

test_that("cpt_stat = var and meanvar are accepted", {
  set.seed(26)
  v <- rnorm(4000, 100, rep(c(1, 4), each = 2000))
  s <- rep(1:400, each = 10)
  for (st in c("mean", "var", "meanvar")) {
    fit <- segmented_qcc(v, "xbar", s, cpt_stat = st, min_seg_len = 2)
    expect_s3_class(fit, "segmented_qcc")
  }
})

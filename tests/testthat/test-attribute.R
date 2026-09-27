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

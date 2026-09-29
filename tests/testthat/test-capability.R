# Capability indices, computed within each segment.

test_that("one row per segment, with the segment's own centre", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  tab <- suppressWarnings(capability_by_segment(fit, spec_limits = c(94, 106)))
  expect_equal(nrow(tab), nrow(fit$segments))
  expect_equal(tab$from, fit$segments$from)
  expect_equal(tab$center, fit$segments$center)
  expect_equal(sum(tab$n_obs), sum(!is.na(fit$data)))
  expect_true(all(is.finite(c(tab$Cp, tab$Cp_k, tab$std_dev))))
})

test_that("Cpk tracks how far off-target each segment is", {
  # segxbar runs at 100, 101.5 and 99.5 against a specification centred on 100,
  # so the middle segment must be the least capable.
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  tab <- suppressWarnings(capability_by_segment(fit, spec_limits = c(94, 106)))
  expect_lt(tab$Cp_k[2], tab$Cp_k[1])
  # Cp ignores the centre, so it barely moves while Cpk swings
  expect_lt(diff(range(tab$Cp)), 0.1)
  expect_gt(diff(range(tab$Cp_k)), diff(range(tab$Cp)))
})

test_that("a centre outside the specification gives a negative Cpk, bounds ordered", {
  # qcc builds the interval as Cpk * (1 +/- ...), which inverts it when Cpk < 0.
  set.seed(61)
  v <- rnorm(2000, 10, 1)
  fit <- segmented_qcc(v, type = "xbar", sample = rep(1:200, each = 10))
  tab <- suppressWarnings(capability_by_segment(fit, spec_limits = c(0, 5)))
  expect_lt(tab$Cp_k[1], 0)
  expect_lte(tab$Cp_k_lwr[1], tab$Cp_k_upr[1])
})

test_that("segments not in control are flagged and warned about", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_warning(tab <- capability_by_segment(fit, spec_limits = c(94, 106)),
                 "not in control")
  expect_equal(tab$in_control, fit$segments$n_out == 0)
})

test_that("the individuals chart is supported and attribute charts are not", {
  ind <- segmented_qcc(segind$value, type = "xbar.one")
  tab <- suppressWarnings(capability_by_segment(ind, spec_limits = c(45, 58)))
  expect_equal(nrow(tab), nrow(ind$segments))

  cc <- segmented_qcc(segc$value, type = "c")
  expect_error(capability_by_segment(cc, spec_limits = c(0, 10)), 'type = "c"')
})

test_that("spec_limits is validated and order does not matter", {
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  expect_error(capability_by_segment(fit, spec_limits = 94), "two finite")
  expect_error(capability_by_segment(fit, spec_limits = c(94, NA)), "two finite")
  expect_error(capability_by_segment(fit, spec_limits = c(94, 94)), "must differ")
  expect_error(capability_by_segment(list(a = 1), c(1, 2)), "segmented_qcc object")
  a <- suppressWarnings(capability_by_segment(fit, c(94, 106)))
  b <- suppressWarnings(capability_by_segment(fit, c(106, 94)))
  expect_equal(a, b)
})

test_that("the plot draws a panel per segment and returns the table", {
  pdf(NULL); on.exit(dev.off())
  fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
  tab <- suppressWarnings(plot_capability_by_segment(fit, c(94, 106)))
  expect_equal(nrow(tab), nrow(fit$segments))
  before <- par("mfrow")
  suppressWarnings(plot_capability_by_segment(fit, c(94, 106)))
  expect_equal(par("mfrow"), before)   # par restored
})

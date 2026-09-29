# Count charts are routed to changepoint's native Poisson cost. A normal cost
# describes a series of small counts badly, since their variance is tied to
# their mean.

test_that("count charts are routed to the Poisson cost, others are not", {
  expect_equal(segmented_qcc(segc$value, type = "c")$test_stat, "Poisson")
  expect_equal(segmented_qcc(rpois(200, 3), type = "u",
                             area = rep(10, 200))$test_stat, "Poisson")
  expect_equal(segmented_qcc(segp$value, type = "p",
                             sizes = segp$sizes)$test_stat, "Normal")
  expect_equal(segmented_qcc(segxbar$value, type = "xbar",
                             sample = segxbar$sample)$test_stat, "Normal")
  expect_equal(segmented_xbar(segxbar$value, segxbar$sample)$test_stat, "Normal")
})

test_that("a u chart with varying area is not treated as Poisson", {
  # The counts then carry the inspection area as well as the rate.
  set.seed(1)
  area <- rep(c(10, 20), 100)
  v <- rpois(200, area * 0.3)
  expect_equal(segmented_qcc(v, type = "u", area = area)$test_stat, "Normal")
  expect_error(segmented_qcc(v, type = "u", area = area,
                             test_stat = "Poisson"), "area is not constant")
})

test_that("Poisson is refused, with the reason, where it cannot apply", {
  expect_error(segmented_qcc(segxbar$value, type = "xbar",
                             sample = segxbar$sample, test_stat = "Poisson"),
               "not a count chart")
  expect_error(segmented_qcc(segp$value, type = "p", sizes = segp$sizes,
                             test_stat = "Poisson"), "not a count chart")
  expect_warning(expect_error(
    segmented_qcc(c(1.5, 2.5, 3.5, 2.5), type = "c", test_stat = "Poisson"),
    "whole, non-negative"))
})

test_that("test_stat = Normal overrides the routing", {
  fit <- segmented_qcc(segc$value, type = "c", test_stat = "Normal")
  expect_equal(fit$test_stat, "Normal")
  expect_error(segmented_qcc(segc$value, type = "c", test_stat = "nope"))
})

test_that("the Poisson cost ignores cpt_stat", {
  expect_warning(fit <- segmented_qcc(segc$value, type = "c",
                                      cpt_stat = "var"), "does not apply")
  expect_equal(fit$test_stat, "Poisson")
})

test_that("min_seg_len is raised silently by default, reported when chosen", {
  # Raising its own default is not news; overriding the caller's choice is.
  expect_silent(segmented_qcc(segc$value, type = "c"))
  expect_warning(fit <- segmented_qcc(segc$value, type = "c",
                                      min_seg_len = 1), "raised from 1 to 2")
  # separate messages, not one run-on string
  expect_true(all(nchar(fit$notes) > 0))
})

test_that("both costs find the bundled Poisson shift", {
  truth <- 150
  for (ts in c("Poisson", "Normal")) {
    fit <- segmented_qcc(segc$value, type = "c", test_stat = ts)
    expect_length(fit$change.points, 1)
    expect_true(abs(fit$change.points[1] - truth) <= 10, info = ts)
  }
})

test_that("the Poisson cost holds up where counts are small", {
  # A homogeneous series of rare events: the normal cost invents change points
  # here, which is the whole reason for the routing.
  set.seed(11)
  v <- rpois(300, 0.2)
  expect_length(segmented_qcc(v, type = "c")$change.points, 0)
})

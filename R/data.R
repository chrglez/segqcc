# Simulated segmented control-chart processes (package data)
# ---------------------------------------------------------------------------
# Three deterministic (seeded) example data sets so the package works out of
# the box without any external files.

#' X-bar segmented example (variable process)
#'
#' Simulated variable process: 400 samples of 8 observations each, built in
#' three segments with means 100, 101.5, 99.5 (SD 2). A mean shift is
#' embedded at samples 120 and 240.
#'
#' @format A data.frame with columns \code{value} (individual observations)
#'   and \code{sample} (sample index).
#' @source Simulated (seed 20260925), see \code{tools/generate_data.R}.
"segxbar"

#' p-chart segmented example (attribute process)
#'
#' Simulated attribute process: 300 samples of size 50 with a defect-rate
#' shift from 0.05 to 0.09 at sample 150.
#'
#' @format A data.frame with columns \code{value} (defectives per sample)
#'   and \code{sizes} (sample size per sample).
#' @source Simulated (seed 777), see \code{tools/generate_data.R}.
"segp"

#' c-chart segmented example (defects per sample)
#'
#' Simulated Poisson defect process: 300 samples with a defect-rate shift
#' from 3 to 5 at sample 150.
#'
#' @format A data.frame with column \code{value} (defects per sample).
#' @source Simulated (seed 4242), see \code{tools/generate_data.R}.
"segc"

#' Individuals segmented example (one observation per sample)
#'
#' Simulated process observed one unit at a time: 400 samples of size 1, in
#' three segments with means 50, 53, 51 (SD 1.5), so the shifts sit at samples
#' 150 and 250. With samples of size 1 there is no within-sample variability to
#' estimate sigma from, so this is the \code{type = "xbar.one"} case.
#'
#' @format A data.frame with one column, \code{value} (the individual
#'   observation of each sample).
#' @source Simulated (seed 31415), see \code{tools/generate_data.R}.
"segind"

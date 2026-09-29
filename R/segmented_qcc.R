#' Segmented change-point control charts
#'
#' Detect change points in a control-chart statistic (via the
#' \code{changepoint} package) and rebuild the chart with the centre and the
#' control limits recomputed within each homogeneous segment. Trending or
#' regime-switching processes are therefore judged against locally appropriate
#' limits instead of a single global set.
#'
#' @param value numeric vector. For variable charts (\code{"xbar"},
#'   \code{"R"}, \code{"S"}) one value per individual observation, grouped by
#'   \code{sample}. For \code{"xbar.one"} and for the attribute charts one
#'   value per sample: the observation itself, or the number of
#'   defectives/defects.
#' @param type control-chart type: \code{"xbar"}, \code{"R"} or \code{"S"}
#'   (variable, grouped samples), \code{"xbar.one"} (individuals, one
#'   observation per sample) or \code{"p"}, \code{"np"}, \code{"c"},
#'   \code{"u"} (attribute).
#' @param sample sample labels, one per element of \code{value}, for variable
#'   charts. Any type coercible to character is accepted; samples are ordered
#'   by \strong{first appearance}, so the data must be in time order. Ignored,
#'   with a warning, for the other chart types.
#' @param sizes per-sample size, required for \code{"p"} and \code{"np"}.
#' @param area per-sample inspection area, required for \code{"u"}.
#' @param method change-point search method: \code{"PELT"} (default),
#'   \code{"AMOC"}, \code{"BinSeg"} or \code{"SegNeigh"}.
#' @param nsigma number of standard errors for the control limits (3).
#' @param penalty penalty passed to \code{changepoint}: one of \code{"None"},
#'   \code{"SIC"}, \code{"BIC"}, \code{"MBIC"} (default), \code{"AIC"},
#'   \code{"Hannan-Quinn"}, \code{"Asymptotic"}, \code{"Manual"},
#'   \code{"CROPS"}.
#' @param pen_value value for the \code{"Asymptotic"}, \code{"Manual"} and
#'   \code{"CROPS"} penalties; ignored otherwise.
#' @param cpt_stat what the detector looks for in the charted statistic:
#'   a change in \code{"mean"} (default, via
#'   \code{\link[changepoint]{cpt.mean}}), in \code{"var"}iance
#'   (\code{\link[changepoint]{cpt.var}}), or in either
#'   (\code{"meanvar"}, \code{\link[changepoint]{cpt.meanvar}}).
#' @param test_stat the cost the detector minimises: \code{"Normal"}, or
#'   \code{"Poisson"} for counts. \code{"auto"} (default) uses
#'   \code{"Poisson"} on a c chart, and on a u chart whose inspection area is
#'   constant, and \code{"Normal"} everywhere else. See Details.
#' @param scale how the charted statistic is standardised before it is handed
#'   to the detector: \code{"mr"} (default) divides by the moving-range
#'   estimate of sigma, \code{"sd"} by the sample standard deviation, and
#'   \code{"none"} passes the statistic through untouched. See Details --
#'   leaving this at \code{"none"} makes the detector scale-dependent.
#' @param run_length length of a run on one side of the centre line that
#'   counts as a violation, as in \code{\link[qcc]{qcc.options}("run.length")}
#'   (7). Runs are counted \strong{within} each segment; see Details. Set to
#'   0 to skip the run rule.
#' @param min_seg_len minimum segment length, in samples (1).
#' @param plot if \code{TRUE}, draws the segmented chart.
#'
#' @return An object of class \code{c("segmented_qcc", "qcc")}: the
#'   \code{\link[qcc]{qcc}} object for the whole series, carrying per-sample
#'   limits, plus
#'   \describe{
#'     \item{\code{change.points}}{sample positions AFTER which the statistic
#'       changed; \code{integer(0)} when the series is homogeneous.}
#'     \item{\code{segments}}{data.frame with one row per segment:
#'       \code{from}, \code{to}, \code{n_samples}, \code{LCL},
#'       \code{center}, \code{UCL}, \code{n_out} and \code{limits_from}
#'       (\code{"segment"}, or \code{"global"} when that segment could not be
#'       refitted on its own).}
#'     \item{\code{out_of_control}}{sample positions whose statistic falls
#'       outside the limits of the segment it belongs to.}
#'     \item{\code{violating_runs}}{sample positions belonging to a run of
#'       \code{run_length} or more consecutive samples on one side of the
#'       centre line, counted within each segment.}
#'     \item{\code{segmented}}{\code{TRUE} if change-point detection ran and
#'       split the series, \code{FALSE} if it was skipped or found nothing.}
#'     \item{\code{notes}}{character vector of the reasons detection or a
#'       per-segment refit degraded, empty when everything succeeded.}
#'   }
#'
#' @details
#' The pipeline is: (1) fit a \code{\link[qcc]{qcc}} chart to the whole series
#' to obtain the per-sample statistic; (2) run the requested \code{changepoint}
#' detector on that statistic; (3) refit the chart within each segment to get
#' locally appropriate limits and centre; (4) rebuild the full chart with those
#' per-sample limits.
#'
#' \strong{Standardisation.} \code{changepoint}'s normal-likelihood cost
#' assumes a unit-variance series, so a statistic on any other scale is
#' over-segmented: on an homogeneous N(100, 2) series of 400 samples
#' \code{cpt.mean} reports about 6 spurious change points, and about 166 when
#' the standard deviation is 20. \code{segmented_qcc} therefore standardises
#' the statistic before detection (\code{scale}), which removes the spurious
#' splits at every scale without costing detection power. Change points are
#' positions, so nothing has to be mapped back: the limits themselves are
#' always recomputed from the original data.
#'
#' \strong{Counts.} A normal cost is a poor description of a series of small
#' counts, whose variance is tied to its mean. \code{changepoint} carries a
#' native Poisson cost, and \code{segmented_qcc} routes count charts to it.
#' It matters where counts are small: on a homogeneous series with a mean of
#' 0.2 the standardised normal cost reports about 3 change points that are not
#' there and the Poisson cost none, and a rise from 0.3 to 1 is found 83\% of
#' the time rather than 52\%. From a mean of about 2 upwards the two agree.
#' The Poisson cost describes the mean and the variance together, so
#' \code{cpt_stat} does not apply to it, and it is fitted to the counts
#' themselves rather than to a standardised statistic.
#'
#' \strong{Run rules.} Besides the samples outside their segment's limits,
#' the chart reports runs of \code{run_length} consecutive samples on one side
#' of the centre line. Unlike \code{\link[qcc]{qcc}}, which counts runs over
#' the whole series, \code{segmented_qcc} counts them within each segment: a
#' run that straddles a change point compares samples against two different
#' centres and is not evidence of anything. The rule itself is
#' \code{qcc}'s, applied segment by segment.
#'
#' \strong{Very short segments.} A segment's limits are estimated from the
#' very samples they then judge, so a segment of one or two samples can never
#' report anything outside them: fitting a segment around an isolated outlier
#' hides it instead of flagging it. This is easy to cause by accident, because
#' the change-point detector will happily isolate a single anomalous sample
#' when \code{min_seg_len} allows it. Such segments are therefore reported as
#' a warning, and the fix is to raise \code{min_seg_len} so that a regime has
#' to last long enough to count as one -- the anomalous samples then surface
#' as ordinary out-of-control points, which is what they are.
#'
#' \strong{Degenerate series.} The function is meant to be pointed at arbitrary
#' series, so it does not fail when step (2) cannot be carried out. If the
#' series is too short for the requested \code{min_seg_len}, if the statistic
#' is constant, or if the detector itself refuses the data, a single-segment
#' chart — an ordinary control chart — is returned with a warning, and the
#' reason is recorded in \code{notes}. The same applies per segment in step
#' (3): a segment that cannot be refitted on its own falls back to the global
#' limits rather than aborting the whole call.
#'
#' @seealso \code{\link{segmented_xbar}} for the X-bar shorthand and
#'   \code{\link{plot_segmented_qcc}} for the plot.
#'
#' @examples
#' # variable chart: 400 samples of 8, with mean shifts at 120 and 240
#' data(segxbar)
#' fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
#' fit
#' fit$segments
#'
#' # individuals chart: one observation per sample
#' data(segind)
#' segmented_qcc(segind$value, type = "xbar.one")
#'
#' # attribute chart
#' data(segc)
#' segmented_qcc(segc$value, type = "c")
#'
#' # a series too short to segment still returns a usable chart
#' suppressWarnings(segmented_qcc(c(3, 5, 4), type = "c"))
#'
#' @export
segmented_qcc <- function(value,
                          type = c("xbar", "R", "S", "xbar.one",
                                   "p", "np", "c", "u"),
                          sample = NULL, sizes = NULL, area = NULL,
                          method = "PELT", nsigma = 3, penalty = "MBIC",
                          pen_value = NULL, cpt_stat = "mean",
                          test_stat = c("auto", "Normal", "Poisson"),
                          scale = c("mr", "sd", "none"),
                          run_length = NULL, min_seg_len = 1, plot = FALSE) {
  type        <- match.arg(type)
  # Only say that min_seg_len was raised if the caller actually chose 1; the
  # Poisson cost raising its own default is not news.
  msl_chosen  <- !missing(min_seg_len)
  method      <- .check_choice(method,   "method",   .CPT_METHODS)
  penalty     <- .check_choice(penalty,  "penalty",  .CPT_PENALTIES)
  cpt_stat    <- .check_choice(cpt_stat, "cpt_stat", .CPT_STATS)
  scale       <- match.arg(scale)
  test_stat   <- match.arg(test_stat)
  nsigma      <- .check_num1(nsigma, "nsigma", min = .Machine$double.eps)
  min_seg_len <- .check_num1(min_seg_len, "min_seg_len", min = 1,
                             integer = TRUE)
  if (is.null(run_length)) run_length <- qcc::qcc.options("run.length")
  run_length <- .check_num1(run_length, "run_length", min = 0, integer = TRUE)
  plot        <- .check_flag(plot, "plot")

  spec <- .prepare_input(type, value, sample, sizes, area)
  K    <- spec$K

  # 1) global chart -> the per-sample statistic the detector works on
  gchart <- .fit_qcc(spec, nsigma = nsigma)
  statistic <- as.numeric(gchart$statistics)
  if (length(statistic) != K)
    stop("internal: qcc returned ", length(statistic), " statistics for ", K,
         " samples. Please report this with your data.", call. = FALSE)

  # 2) change-point detection, degrading to a single segment when impossible
  res <- .resolve_test_stat(test_stat, spec)
  det <- .detect_change_points(statistic, spec$value, method, penalty,
                               pen_value, cpt_stat, res$test_stat, scale,
                               min_seg_len, K, msl_chosen)
  det$notes <- c(res$notes, det$notes)
  cpts_idx <- det$cpts
  notes    <- det$notes

  # 3) per-segment refit -> locally appropriate limits
  bounds <- c(0L, cpts_idx, K)
  segs <- data.frame(from = bounds[-length(bounds)] + 1L, to = bounds[-1L])
  segs$n_samples <- segs$to - segs$from + 1L

  gl  <- .limits_of(gchart)
  fits <- lapply(seq_len(nrow(segs)), function(i)
    .refit_segment(spec, nsigma, segs$from[i], segs$to[i], i, gl))
  segs$LCL         <- vapply(fits, function(x) x$LCL,    numeric(1))
  segs$center      <- vapply(fits, function(x) x$center, numeric(1))
  segs$UCL         <- vapply(fits, function(x) x$UCL,    numeric(1))
  segs$limits_from <- vapply(fits, function(x) x$source, character(1))
  notes <- c(notes, unlist(lapply(fits, function(x) x$note), use.names = FALSE))

  # 4) final chart: whole series, per-segment limits and centres
  LCL <- rep(segs$LCL,    segs$n_samples)
  UCL <- rep(segs$UCL,    segs$n_samples)
  ctr <- rep(segs$center, segs$n_samples)

  out <- .fit_qcc(spec, nsigma = nsigma, limits = cbind(LCL, UCL), center = ctr)

  oc <- which((!is.na(UCL) & statistic > UCL) | (!is.na(LCL) & statistic < LCL))
  runs <- .runs_by_segment(statistic, ctr, segs, run_length)
  count_in <- function(pos) vapply(seq_len(nrow(segs)), function(i)
    sum(pos >= segs$from[i] & pos <= segs$to[i]), integer(1))
  segs$n_out  <- count_in(oc)
  segs$n_runs <- count_in(runs)
  segs <- segs[c("from", "to", "n_samples", "LCL", "center", "UCL",
                 "n_out", "n_runs", "limits_from")]

  notes <- c(notes, .short_segment_notes(segs, min_seg_len))

  out$change.points  <- as.integer(cpts_idx)
  out$segments       <- segs
  out$out_of_control <- unname(oc)
  out$violating_runs <- runs
  # qcc computed its own runs over the whole series, ignoring the segment
  # boundaries; replace them so anything reading the object sees the
  # segment-aware ones.
  out$violations$violating.runs <- runs
  out$run_length     <- run_length
  out$test_stat      <- res$test_stat
  out$segmented      <- length(cpts_idx) > 0L
  out$notes          <- notes
  out$chart.type     <- type
  out$n_samples      <- K
  out$sample.labels  <- spec$labels
  out$call           <- match.call()
  class(out) <- c("segmented_qcc", class(out))

  for (n in notes) warning(n, call. = FALSE)
  if (plot) plot_segmented_qcc(out)
  out
}

#' Segmented X-bar control chart
#'
#' Shorthand for the variable X-bar chart. Equivalent to
#' \code{segmented_qcc(value, type = "xbar", sample = sample, ...)}; see
#' \code{\link{segmented_qcc}} for the details and the return value.
#'
#' @param value numeric vector of individual observations.
#' @param sample sample labels, one per observation.
#' @inheritParams segmented_qcc
#'
#' @return An object of class \code{c("segmented_qcc", "qcc")}.
#'
#' @examples
#' data(segxbar)
#' fit <- segmented_xbar(segxbar$value, segxbar$sample)
#' fit$segments
#'
#' @export
segmented_xbar <- function(value, sample, method = "PELT", nsigma = 3,
                           penalty = "MBIC", pen_value = NULL,
                           cpt_stat = "mean", scale = c("mr", "sd", "none"),
                           run_length = NULL, min_seg_len = 1, plot = FALSE) {
  # the xbar chart is never a count chart, so the detector stays Normal
  segmented_qcc(value, type = "xbar", sample = sample, method = method,
                nsigma = nsigma, penalty = penalty, pen_value = pen_value,
                cpt_stat = cpt_stat, test_stat = "Normal",
                scale = match.arg(scale),
                run_length = run_length, min_seg_len = min_seg_len,
                plot = plot)
}

# ---- internals --------------------------------------------------------------

# Fit a qcc chart for the series described by `spec`, optionally restricted to
# the sample positions `rows` and optionally with explicit per-sample limits.
.fit_qcc <- function(spec, nsigma, rows = NULL, limits = NULL, center = NULL) {
  args <- list(type = spec$type, nsigmas = nsigma, plot = FALSE)
  if (!is.null(limits)) args$limits <- limits
  if (!is.null(center)) args$center <- center

  if (spec$family == "variable") {
    data <- spec$groups
    if (!is.null(rows)) data <- spec$groups[rows, , drop = FALSE]
    do.call(qcc::qcc, c(list(data), args))
  } else {
    v <- spec$value
    if (!is.null(rows)) v <- v[rows]
    if (spec$type %in% c("p", "np")) {
      sz <- if (is.null(rows)) spec$sizes else spec$sizes[rows]
      do.call(qcc::qcc, c(list(v), list(sizes = sz), args))
    } else if (spec$type == "u") {
      ar <- if (is.null(rows)) spec$area else spec$area[rows]
      do.call(qcc::qcc, c(list(v), list(sizes = ar), args))
    } else { # c, xbar.one
      do.call(qcc::qcc, c(list(v), args))
    }
  }
}

# qcc reports limits either as a length-2 vector or as a K x 2 matrix; reduce
# to the single (LCL, UCL) pair of a chart fitted on homogeneous data.
.limits_of <- function(chart) {
  lim <- chart$limits
  if (is.matrix(lim)) lim <- lim[1L, ]
  list(LCL = as.numeric(lim[1L]), UCL = as.numeric(lim[2L]),
       center = as.numeric(chart$center)[1L])
}

# Refit one segment. A segment that qcc cannot fit on its own (too few samples
# for a moving range, a constant statistic, ...) falls back to the limits of
# the global chart rather than aborting the call.
.refit_segment <- function(spec, nsigma, from, to, i, global) {
  fallback <- function(why)
    list(LCL = global$LCL, center = global$center, UCL = global$UCL,
         source = "global",
         note = paste0("segment ", i, " (samples ", from, "-", to, ", ",
                       to - from + 1L, " sample(s)) ", why,
                       "; used the limits of the whole series instead"))

  fit <- tryCatch(
    .fit_qcc(spec, nsigma = nsigma, rows = from:to),
    error   = function(e) structure(conditionMessage(e), class = "cpt_failed"),
    warning = function(w) structure(conditionMessage(w), class = "cpt_failed"))
  if (inherits(fit, "cpt_failed"))
    return(fallback(paste0("could not be refitted (", fit, ")")))

  lim <- .limits_of(fit)
  if (!all(is.finite(c(lim$LCL, lim$UCL, lim$center))))
    return(fallback("has no finite control limits of its own"))
  if (isTRUE(all.equal(lim$LCL, lim$UCL)))
    return(list(LCL = lim$LCL, center = lim$center, UCL = lim$UCL,
                source = "segment",
                note = paste0("segment ", i, " (samples ", from, "-", to,
                              ") has zero dispersion, so its control limits ",
                              "collapse onto the centre")))
  c(lim[c("LCL", "center", "UCL")], list(source = "segment", note = NULL))
}

# A segment's control limits are estimated from its own samples, so a segment
# of one or two samples cannot place any of them outside its limits: an
# isolated outlier given a segment of its own stops being a signal. Warn,
# because nothing else in the output shows that a reported "in control"
# segment was never able to report anything else. One note for all of them:
# an over-segmented series can produce hundreds, and one warning each would
# bury everything else.
.short_segment_notes <- function(segs, min_seg_len) {
  short <- which(segs$n_samples < 3L)
  if (!length(short) || nrow(segs) == 1L) return(character(0))
  shown <- if (length(short) > 6L)
             paste0(paste(short[1:6], collapse = ", "), ", ... (",
                    length(short) - 6L, " more)")
           else paste(short, collapse = ", ")
  paste0(length(short), " of the ", nrow(segs), " segments have fewer than 3 ",
         "samples (segment ", shown, "), so their limits are estimated from ",
         "the very samples they judge and nothing in them can come out as out ",
         "of control. Raise min_seg_len (now ", min_seg_len, ") if these are ",
         "isolating anomalous samples rather than real regimes.")
}

# Decide which cost the detector minimises. "auto" picks the Poisson cost for
# the count charts it actually fits, and says so in a note when it does; an
# explicit choice is honoured, or refused with the reason.
.resolve_test_stat <- function(test_stat, spec) {
  ok <- .poisson_eligible(spec)
  if (test_stat == "Normal") return(list(test_stat = "Normal", notes = character(0)))
  if (test_stat == "Poisson") {
    if (ok) return(list(test_stat = "Poisson", notes = character(0)))
    why <- if (!spec$type %in% c("c", "u"))
             paste0('type = "', spec$type, '" is not a count chart')
           else if (spec$type == "u")
             "the u chart's inspection area is not constant, so its counts
              carry the area as well as the rate"
           else "the counts are not whole, non-negative numbers"
    stop('test_stat = "Poisson" needs a series of Poisson counts, but ',
         gsub("\\s+", " ", why), '. Use test_stat = "Normal".', call. = FALSE)
  }
  if (!ok) return(list(test_stat = "Normal", notes = character(0)))
  list(test_stat = "Poisson", notes = character(0))
}

# Samples belonging to a run of `run_length` consecutive points on one side of
# the centre line, counted within each segment. qcc's own rule is applied to
# each segment in turn, so the semantics stay exactly qcc's; what changes is
# that a run is not allowed to straddle a change point, where the centre it is
# measured against is a different number.
.runs_by_segment <- function(statistic, center, segs, run_length) {
  if (run_length <= 0L) return(integer(0))
  hits <- lapply(seq_len(nrow(segs)), function(i) {
    idx <- segs$from[i]:segs$to[i]
    v <- qcc::violating.runs(
      list(statistics = statistic[idx], center = center[idx][1L],
           newstats = NULL, limits = NULL),
      run.length = run_length)
    if (length(v)) idx[v] else integer(0)
  })
  sort(unique(as.integer(unlist(hits, use.names = FALSE))))
}

# Locate change points in the charted statistic. Never raises: anything that
# prevents detection is reported as a note and yields no change points, so the
# caller gets an ordinary single-segment control chart.
.detect_change_points <- function(statistic, counts, method, penalty, pen_value,
                                  cpt_stat, test_stat, scale, min_seg_len, K,
                                  msl_chosen = TRUE) {
  notes <- character(0)
  none <- function(why) list(cpts = integer(0), notes = c(notes, why))
  if (is.null(counts)) counts <- statistic

  # The Poisson cost cannot work with one-sample segments, so settle the
  # effective minimum before anything is checked against it.
  if (test_stat == "Poisson") {
    if (cpt_stat != "mean")
      notes <- c(notes, paste0('cpt_stat = "', cpt_stat, '" does not apply to ',
                 'the Poisson cost, which describes the mean and the variance ',
                 "together; it was ignored"))
    if (min_seg_len < 2L) {
      if (msl_chosen)
        notes <- c(notes, paste("the Poisson cost needs segments of at least",
                                "2 samples; min_seg_len was raised from 1 to 2"))
      min_seg_len <- 2L
    }
  }

  if (K < 2L * min_seg_len)
    return(none(paste0("the series has ", K, " sample(s) but min_seg_len = ",
                       min_seg_len, " needs at least ", 2L * min_seg_len,
                       " for a split; change-point detection was skipped and ",
                       "a single segment returned")))
  if (!any(is.finite(statistic)))
    return(none("the charted statistic has no finite values; change-point
                 detection was skipped"))
  if (stats::var(statistic) == 0)
    return(none(paste0("the charted statistic is constant (", statistic[1L],
                       ") across all ", K, " samples, so there is no change ",
                       "to detect; a single segment was returned")))

  # changepoint's normal cost assumes unit variance, so the statistic has to be
  # put on that scale or the detector over-segments in proportion to the series'
  # own dispersion. The moving-range estimate is the SPC standard and, unlike
  # sd(), is not inflated by the very level shifts we are looking for.
  z <- statistic - mean(statistic)
  s_hat <- switch(scale,
                  mr   = mean(abs(diff(statistic))) / 1.128,
                  sd   = stats::sd(statistic),
                  none = 1)
  if (is.finite(s_hat) && s_hat > 0) z <- z / s_hat

  if (test_stat == "Poisson") {
    # The Poisson cost is fitted to the counts themselves: standardising them
    # would leave a series the cost cannot describe.
    detector <- changepoint::cpt.meanvar
    args <- list(counts, method = method, penalty = penalty,
                 minseglen = min_seg_len, test.stat = "Poisson")
  } else {
    detector <- switch(cpt_stat,
                       mean    = changepoint::cpt.mean,
                       var     = changepoint::cpt.var,
                       meanvar = changepoint::cpt.meanvar)
    args <- list(z, method = method, penalty = penalty,
                 minseglen = min_seg_len)
  }
  if (!is.null(pen_value)) args$pen.value <- pen_value

  fit <- tryCatch(do.call(detector, args),
                  error = function(e)
                    structure(conditionMessage(e), class = "cpt_failed"))
  if (inherits(fit, "cpt_failed"))
    return(none(paste0('change-point detection failed for method = "', method,
                       '", penalty = "', penalty, '", cpt_stat = "', cpt_stat,
                       '" on ', K, " samples (", fit,
                       "); a single segment was returned")))

  cpts <- tryCatch(as.integer(changepoint::cpts(fit)),
                   error = function(e) integer(0))
  # changepoint occasionally reports the final point as a change point, which
  # would create an empty trailing segment.
  cpts <- sort(unique(cpts[is.finite(cpts) & cpts >= 1L & cpts < K]))
  list(cpts = cpts, notes = notes)
}

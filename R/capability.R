#' Process capability by segment
#'
#' Capability indices computed \emph{within each segment} of a segmented
#' chart, against a pair of specification limits.
#'
#' @param fit object returned by \code{\link{segmented_qcc}} or
#'   \code{\link{segmented_xbar}}, of type \code{"xbar"} or \code{"xbar.one"}.
#' @param spec_limits length-2 numeric vector, the lower and upper
#'   specification limits.
#' @param target the target value; defaults to the midpoint of
#'   \code{spec_limits}. Only \code{Cpm} depends on it.
#' @param conf_level confidence level for the interval of each index (0.95).
#'
#' @return A data.frame with one row per segment: \code{from}, \code{to},
#'   \code{n_samples}, \code{n_obs}, \code{center}, \code{std_dev}, the indices
#'   \code{Cp}, \code{Cp_l}, \code{Cp_u}, \code{Cp_k} and \code{Cpm}, the
#'   confidence bounds \code{Cp_k_lwr} and \code{Cp_k_upr}, the expected
#'   fractions outside each specification limit (\code{exp_below},
#'   \code{exp_above}), and \code{in_control} — \code{FALSE} when the segment
#'   has samples beyond its own limits, which makes its indices unreliable.
#'
#' @details
#' Capability indices describe a \strong{stable} process: they compare the
#' spread of a single process against the specification. Computed over a series
#' that changes regime they describe no process at all, mixing several. A
#' segment is the homogeneous unit, so this is where the indices belong — and
#' reading them segment by segment answers the question a single global index
#' cannot: \emph{when} the process was capable.
#'
#' Two caveats are reported rather than assumed away. A segment holding samples
#' outside its own control limits is not in control, so its indices are flagged
#' with \code{in_control = FALSE}; and a segment with few observations gives
#' imprecise indices, which is what the confidence bounds on \code{Cp_k} are
#' for. The indices themselves are \code{\link[qcc]{process.capability}}'s,
#' computed on each segment's own data.
#'
#' @seealso \code{\link{plot_capability_by_segment}} for the matching plot.
#'
#' @examples
#' data(segxbar)
#' fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
#' capability_by_segment(fit, spec_limits = c(94, 106))
#'
#' @export
capability_by_segment <- function(fit, spec_limits, target = NULL,
                                  conf_level = 0.95) {
  spec <- .check_spec_limits(fit, spec_limits)
  if (is.null(target)) target <- mean(spec)
  target    <- .check_num1(target, "target")
  conf_level <- .check_num1(conf_level, "conf_level", min = 1e-6, max = 1 - 1e-6)

  segs <- fit$segments
  rows <- lapply(seq_len(nrow(segs)), function(i)
    .capability_one(fit, segs[i, ], spec, target, conf_level))
  out <- do.call(rbind, rows)

  bad <- which(!out$in_control)
  if (length(bad))
    warning("segment(s) ", paste(bad, collapse = ", "),
            " hold samples outside their own control limits, so they are not ",
            "in control and their capability indices do not describe a stable ",
            "process", call. = FALSE)
  out
}

#' Plot process capability by segment
#'
#' One panel per segment: the distribution of that segment's observations
#' against the specification limits, with its \eqn{C_{pk}} annotated. Panels
#' share their axes, so the segments can be read against each other.
#'
#' @inheritParams capability_by_segment
#' @param main overall title.
#' @param ncol number of panel columns; the default lays the segments out in a
#'   roughly square grid.
#'
#' @return The capability table, invisibly, as returned by
#'   \code{\link{capability_by_segment}}.
#'
#' @examples
#' data(segxbar)
#' fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
#' plot_capability_by_segment(fit, spec_limits = c(94, 106))
#'
#' @importFrom graphics hist abline box mtext par
#' @importFrom stats dnorm
#' @export
plot_capability_by_segment <- function(fit, spec_limits, target = NULL,
                                       conf_level = 0.95, main = NULL,
                                       ncol = NULL) {
  tab  <- capability_by_segment(fit, spec_limits, target, conf_level)
  spec <- .check_spec_limits(fit, spec_limits)
  n    <- nrow(tab)
  if (is.null(ncol)) ncol <- ceiling(sqrt(n))
  nrow_ <- ceiling(n / ncol)
  if (is.null(main))
    main <- paste0("Capability by segment - ", fit$chart.type, " chart")

  op <- par(mfrow = c(nrow_, ncol), mar = c(3.4, 3.2, 2.6, 0.8),
            oma = c(0, 0, 2.4, 0), mgp = c(2, 0.7, 0))
  on.exit(par(op), add = TRUE)

  obs <- lapply(seq_len(n), function(i)
    .segment_observations(fit, tab$from[i], tab$to[i]))
  # A handful of extreme observations would squash every panel against one
  # edge, so the view spans the specification and four sigma either side of
  # each segment's centre rather than the full data range.
  xlim <- range(c(spec, tab$center - 4 * tab$std_dev,
                  tab$center + 4 * tab$std_dev), finite = TRUE)

  for (i in seq_len(n)) {
    h <- hist(obs[[i]], breaks = "scott", plot = FALSE)
    dens <- dnorm(seq(xlim[1], xlim[2], length.out = 200),
                  tab$center[i], tab$std_dev[i])
    hist(obs[[i]], breaks = "scott", freq = FALSE, xlim = xlim,
         ylim = c(0, max(h$density, dens, na.rm = TRUE) * 1.1),
         col = if (tab$in_control[i]) "gray85" else "mistyrose",
         border = "white", xlab = "", ylab = "Density", main = "")
    mtext(paste0("Segment ", i, " (", tab$from[i], "-", tab$to[i], ")"),
          side = 3, line = 1.1, font = 2, cex = 0.85)
    points(seq(xlim[1], xlim[2], length.out = 200), dens, type = "l",
           col = "gray30")
    abline(v = spec, col = "red", lty = 2, lwd = 1.4)
    abline(v = tab$center[i], col = "blue", lwd = 1.3)
    mtext(paste0("Cpk = ", format(tab$Cp_k[i], digits = 3),
                 if (!tab$in_control[i]) " - not in control" else ""),
          side = 3, line = 0.15, cex = 0.7,
          col = if (tab$in_control[i]) "gray25" else "red3")
    box()
  }
  mtext(main, outer = TRUE, cex = 1.05, font = 2)
  invisible(tab)
}

# ---- internals --------------------------------------------------------------

.check_spec_limits <- function(fit, spec_limits) {
  if (!inherits(fit, "segmented_qcc"))
    stop("`fit` must be a segmented_qcc object, as returned by ",
         "segmented_qcc()", call. = FALSE)
  if (!fit$chart.type %in% c("xbar", "xbar.one"))
    stop("capability indices compare a measurement against a specification, ",
         'so they are defined here only for type = "xbar" and "xbar.one"; ',
         'this chart is type = "', fit$chart.type, '"', call. = FALSE)
  if (!is.numeric(spec_limits) || length(spec_limits) != 2L ||
      anyNA(spec_limits) || !all(is.finite(spec_limits)))
    stop("`spec_limits` must be two finite numbers, the lower and upper ",
         "specification limits", call. = FALSE)
  spec <- sort(as.numeric(spec_limits))
  if (spec[1L] == spec[2L])
    stop("`spec_limits` must differ from each other", call. = FALSE)
  spec
}

# The individual observations of one segment: the rows of the group matrix for
# a grouped chart, the raw values for an individuals chart.
.segment_observations <- function(fit, from, to) {
  d <- fit$data
  if (is.matrix(d)) as.numeric(d[from:to, , drop = FALSE]) else as.numeric(d[from:to])
}

.capability_one <- function(fit, seg, spec, target, conf_level) {
  obs <- .segment_observations(fit, seg$from, seg$to)
  obs <- obs[!is.na(obs)]
  chart <- list(type = fit$chart.type, center = seg$center,
                std.dev = fit$std.dev, nsigmas = fit$nsigmas,
                data = if (fit$chart.type == "xbar.one") obs else NULL,
                sizes = NULL, statistics = NULL)
  # Recompute the segment's own sigma from its own data rather than reuse the
  # whole series': a segment's capability has to be its own.
  sigma <- .segment_sigma(fit, seg)

  cap <- tryCatch({
    q <- .refit_for_capability(fit, seg)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    qcc::process.capability(q, spec.limits = spec, target = target,
                            confidence.level = conf_level, print = FALSE)
  }, error = function(e) NULL)

  idx <- if (is.null(cap)) rep(NA_real_, 5L) else cap$indices[, 1L]
  # qcc builds the Cpk interval as Cpk * (1 +/- ...), which puts the bounds the
  # wrong way round when Cpk is negative - a centre outside the specification.
  bounds <- if (is.null(cap)) c(NA_real_, NA_real_)
            else sort(c(cap$indices[4L, 2L], cap$indices[4L, 3L]))
  lwr <- bounds[1L]; upr <- bounds[2L]
  expd <- if (is.null(cap)) c(NA_real_, NA_real_) else cap$exp

  data.frame(
    from = seg$from, to = seg$to, n_samples = seg$n_samples,
    n_obs = length(obs), center = seg$center,
    std_dev = if (is.null(cap)) sigma else cap$std.dev,
    Cp = idx[1L], Cp_l = idx[2L], Cp_u = idx[3L], Cp_k = idx[4L],
    Cpm = idx[5L], Cp_k_lwr = lwr, Cp_k_upr = upr,
    exp_below = expd[1L], exp_above = expd[2L],
    in_control = seg$n_out == 0L,
    row.names = NULL)
}

# Refit the plain qcc chart for one segment, which is what process.capability
# consumes.
.refit_for_capability <- function(fit, seg) {
  d <- fit$data
  rows <- seg$from:seg$to
  if (is.matrix(d))
    qcc::qcc(d[rows, , drop = FALSE], type = fit$chart.type,
             nsigmas = fit$nsigmas, plot = FALSE)
  else
    qcc::qcc(d[rows], type = fit$chart.type, nsigmas = fit$nsigmas,
             plot = FALSE)
}

# Sigma of a segment, used only as a fallback when qcc's own routine refuses
# the segment.
.segment_sigma <- function(fit, seg) {
  obs <- .segment_observations(fit, seg$from, seg$to)
  stats::sd(obs[!is.na(obs)])
}

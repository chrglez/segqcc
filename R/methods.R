#' Plot a segmented control chart
#'
#' Draws the per-sample statistic with each segment's centre and control limits
#' spanning \emph{only} the samples of that segment, and the detected change
#' points as vertical rules. Samples outside the limits of their own segment
#' are marked in red, and samples belonging to a run on one side of their
#' segment's centre line in orange, following the convention of
#' \code{\link[qcc]{plot.qcc}}.
#'
#' @param fit object returned by \code{\link{segmented_qcc}} or
#'   \code{\link{segmented_xbar}}.
#' @param main plot title; the default names the chart type.
#' @param xlab,ylab axis labels; the default \code{ylab} names the statistic of
#'   the chart type.
#' @param show_legend if \code{TRUE} (default), adds a legend.
#'
#' @return \code{fit}, invisibly.
#'
#' @examples
#' data(segxbar)
#' fit <- segmented_qcc(segxbar$value, type = "xbar", sample = segxbar$sample)
#' plot_segmented_qcc(fit)
#'
#' @importFrom graphics par points segments abline legend mtext
#' @export
plot_segmented_qcc <- function(fit, main = NULL, xlab = "Sample", ylab = NULL,
                               show_legend = TRUE) {
  if (!inherits(fit, "segmented_qcc"))
    stop("`fit` must be a segmented_qcc object, as returned by ",
         "segmented_qcc()", call. = FALSE)
  type  <- fit$chart.type
  stats <- as.numeric(fit$statistics)
  segs  <- fit$segments
  if (is.null(main)) main <- paste0("Segmented ", type, " chart")
  if (is.null(ylab)) ylab <- .stat_label(type)

  op <- par(mar = c(4.2, 4.2, 3.2, 1.2))
  on.exit(par(op), add = TRUE)

  rng <- range(c(stats, segs$LCL, segs$UCL, segs$center),
               na.rm = TRUE, finite = TRUE)
  pad <- diff(rng) * 0.08
  if (!is.finite(pad) || pad == 0) pad <- max(abs(rng[1L]) * 0.1, 1)
  # The legend is drawn inside the panel, so leave it room rather than let it
  # sit on top of the first samples.
  plot(seq_along(stats), stats, type = "n", xlab = xlab, ylab = ylab,
       main = main,
       ylim = c(rng[1L] - pad, rng[2L] + pad * if (show_legend) 3 else 1))

  # Each segment's limits are drawn only across that segment: spanning them
  # over the whole width (abline) would suggest limits that never applied.
  for (i in seq_len(nrow(segs))) {
    x0 <- segs$from[i] - 0.5; x1 <- segs$to[i] + 0.5
    segments(x0, segs$center[i], x1, segs$center[i], col = "blue", lwd = 1.6)
    segments(x0, segs$LCL[i], x1, segs$LCL[i], col = "red", lty = 2, lwd = 1.4)
    segments(x0, segs$UCL[i], x1, segs$UCL[i], col = "red", lty = 2, lwd = 1.4)
  }
  if (length(fit$change.points))
    abline(v = fit$change.points + 0.5, col = "gray40", lwd = 2, lty = 3)

  lines_x <- seq_along(stats)
  points(lines_x, stats, type = "l", col = "gray65")
  points(lines_x, stats, pch = 16, cex = 0.6)
  # Run violations first, so a sample that is both keeps the stronger red.
  if (length(fit$violating_runs))
    points(fit$violating_runs, stats[fit$violating_runs], pch = 16,
           col = "orange", cex = 1.0)
  if (length(fit$out_of_control))
    points(fit$out_of_control, stats[fit$out_of_control], pch = 16,
           col = "red", cex = 1.1)

  if (!fit$segmented)
    mtext("no change points detected - single segment", side = 3, line = 0.2,
          cex = 0.8, col = "gray35")
  if (show_legend)
    legend("topleft", bty = "n", cex = 0.72, horiz = TRUE,
           legend = c("centre", "limits", "change point", "beyond limits",
                      "violating run"),
           col = c("blue", "red", "gray40", "red", "orange"),
           lty = c(1, 2, 3, NA, NA), pch = c(NA, NA, NA, 16, 16))
  invisible(fit)
}

#' @rdname plot_segmented_qcc
#' @param x a \code{segmented_qcc} object.
#' @param ... further arguments passed to \code{plot_segmented_qcc}.
#' @export
plot.segmented_qcc <- function(x, ...) plot_segmented_qcc(x, ...)

#' Print a segmented control chart
#'
#' @param x a \code{segmented_qcc} object.
#' @param digits number of significant digits for the segment table.
#' @param ... ignored.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' data(segc)
#' segmented_qcc(segc$value, type = "c")
#'
#' @export
print.segmented_qcc <- function(x, digits = 4, ...) {
  cat("Segmented ", x$chart.type, " control chart\n", sep = "")
  cat(strrep("-", 40), "\n", sep = "")
  cat("Samples:        ", x$n_samples, "\n", sep = "")
  cat("Change points:  ",
      if (length(x$change.points)) paste(x$change.points, collapse = ", ")
      else "none detected", "\n", sep = "")
  cat("Segments:       ", nrow(x$segments), "\n", sep = "")
  cat("Beyond limits:  ", length(x$out_of_control), " of ", x$n_samples,
      " samples (", format(100 * length(x$out_of_control) / x$n_samples,
                           digits = 2), "%)\n", sep = "")
  cat("Violating runs: ", length(x$violating_runs), " samples (runs of ",
      x$run_length, "+ on one side of the centre, within a segment)\n",
      sep = "")
  cat("\n")
  print(.format_segments(x$segments, digits))
  if (length(x$notes)) {
    cat("\nNotes:\n")
    for (n in x$notes) cat(" -", n, "\n")
  }
  invisible(x)
}

#' Summarise a segmented control chart
#'
#' Adds the per-segment out-of-control samples to what \code{print()} shows.
#'
#' @param object a \code{segmented_qcc} object.
#' @param digits number of significant digits.
#' @param ... ignored.
#'
#' @return \code{object}, invisibly.
#'
#' @examples
#' data(segxbar)
#' summary(segmented_xbar(segxbar$value, segxbar$sample))
#'
#' @export
summary.segmented_qcc <- function(object, digits = 4, ...) {
  print(object, digits = digits)
  cat("\nStandard deviation (whole series): ",
      format(object$std.dev, digits = digits), "\n", sep = "")
  for (what in c("out_of_control", "violating_runs")) {
    pos <- object[[what]]
    if (!length(pos)) next
    cat("\n", if (what == "out_of_control") "Samples beyond the limits"
        else "Samples in a violating run", ", by segment:\n", sep = "")
    for (i in seq_len(nrow(object$segments))) {
      s <- object$segments[i, ]
      hit <- pos[pos >= s$from & pos <= s$to]
      if (length(hit))
        cat("  segment ", i, " (", s$from, "-", s$to, "): ",
            paste(hit, collapse = ", "), "\n", sep = "")
    }
  }
  invisible(object)
}

# Label for the charted statistic, used on the y axis.
.stat_label <- function(type)
  switch(type,
         xbar       = expression(bar(x)),
         xbar.one   = "Individual value",
         R          = "Range",
         S          = "Std. deviation",
         p          = "Proportion defective",
         np         = "Number defective",
         c          = "Defects per sample",
         u          = "Defects per unit",
         "Statistic")

# Round the numeric columns of the segment table for display, leaving the
# integer bookkeeping columns alone.
.format_segments <- function(segs, digits) {
  out <- segs
  for (j in c("LCL", "center", "UCL"))
    out[[j]] <- signif(out[[j]], digits)
  out
}

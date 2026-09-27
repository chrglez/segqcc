# Internal validation and input normalisation for segmented_qcc().
#
# Design rule for every message in this file: name the offending argument and,
# when a working alternative exists, name it too. The package is meant to be
# pointed at arbitrary series, so a caller must be able to tell from the
# message alone whether the data or the call is at fault.

.VARIABLE_TYPES  <- c("xbar", "R", "S")
.INDIV_TYPES     <- "xbar.one"
.ATTRIBUTE_TYPES <- c("p", "np", "c", "u")
.ALL_TYPES       <- c(.VARIABLE_TYPES, .INDIV_TYPES, .ATTRIBUTE_TYPES)

.CPT_METHODS   <- c("PELT", "AMOC", "BinSeg", "SegNeigh")
.CPT_PENALTIES <- c("None", "SIC", "BIC", "MBIC", "AIC", "Hannan-Quinn",
                    "Asymptotic", "Manual", "CROPS")
.CPT_STATS     <- c("mean", "meanvar", "var")

# qcc tabulates the d2/d3 constants behind the R chart only up to n = 25, and
# errors outright from n = 51. Beyond 25 it silently returns NA limits, so we
# stop first and point at the S chart, which has a closed form for any n.
.R_MAX_N <- 25L

# ---- scalar argument checks -------------------------------------------------

.check_num1 <- function(x, name, min = -Inf, max = Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x))
    stop("`", name, "` must be a single finite number", call. = FALSE)
  if (integer && x != round(x))
    stop("`", name, "` must be a whole number, not ", x, call. = FALSE)
  if (x < min || x > max)
    stop("`", name, "` must be in [", min, ", ", max, "], not ", x,
         call. = FALSE)
  if (integer) as.integer(x) else as.numeric(x)
}

.check_choice <- function(x, name, choices) {
  if (!is.character(x) || length(x) != 1L || is.na(x))
    stop("`", name, "` must be a single string; one of ",
         paste0('"', choices, '"', collapse = ", "), call. = FALSE)
  hit <- choices[match(x, choices)]
  if (is.na(hit))
    stop('`', name, '` = "', x, '" is not supported; use one of ',
         paste0('"', choices, '"', collapse = ", "), call. = FALSE)
  hit
}

.check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop("`", name, "` must be TRUE or FALSE", call. = FALSE)
  x
}

# ---- count vectors (value / sizes / area) ----------------------------------

# Counts and areas must be usable by qcc: present, numeric, non-negative. The
# integer check is a warning rather than an error because a u chart legitimately
# takes non-integer areas, and some users carry counts as doubles.
.check_counts <- function(x, name, K = NULL, positive = FALSE,
                          whole = TRUE, what = "value") {
  if (is.null(x))
    stop("`", name, "` is required for this chart type", call. = FALSE)
  if (!is.numeric(x))
    stop("`", name, "` must be numeric, not ", class(x)[1L], call. = FALSE)
  if (anyNA(x))
    stop("`", name, "` must not contain NA (", sum(is.na(x)), " of ",
         length(x), " values are NA); change-point detection needs a ",
         "complete, regularly spaced series", call. = FALSE)
  if (!all(is.finite(x)))
    stop("`", name, "` must not contain Inf or NaN", call. = FALSE)
  if (!is.null(K) && length(x) != K)
    stop("`", name, "` has length ", length(x), " but the series has ", K,
         " samples; they must match one-to-one", call. = FALSE)
  if (positive && any(x <= 0))
    stop("`", name, "` must be strictly positive; found ", sum(x <= 0),
         " value(s) <= 0", call. = FALSE)
  if (!positive && any(x < 0))
    stop("`", name, "` must not be negative; found ", sum(x < 0),
         " negative value(s)", call. = FALSE)
  if (whole && any(x != round(x)))
    warning("`", name, "` holds non-integer ", what,
            "s; they are counts for this chart type and will be used as given",
            call. = FALSE)
  as.numeric(x)
}

# ---- input normalisation ---------------------------------------------------

# Turn the user's arguments into a single internal description of the series.
# Returns a list with:
#   type    chart type
#   family  "variable" | "individual" | "attribute"
#   K       number of samples (the length of the charted statistic)
#   groups  K x nmax matrix of observations, NA-padded (variable charts only)
#   value   the series as qcc wants it (individual / attribute charts)
#   sizes   per-sample size    (p, np)
#   area    per-sample area    (u)
#   labels  sample labels in time order, for axis annotation
.prepare_input <- function(type, value, sample, sizes, area) {
  family <- if (type %in% .VARIABLE_TYPES) "variable"
            else if (type %in% .INDIV_TYPES) "individual"
            else "attribute"

  if (!is.numeric(value) && !is.logical(value))
    stop("`value` must be numeric, not ", class(value)[1L], call. = FALSE)
  value <- as.numeric(value)
  if (length(value) == 0L)
    stop("`value` is empty; nothing to chart", call. = FALSE)

  if (family == "variable")
    return(.prepare_variable(type, value, sample))

  # From here on each element of `value` is one sample, so a stray `sample`
  # argument means the caller expects grouping that will not happen.
  if (!is.null(sample))
    warning('`sample` is ignored for type = "', type,
            '": each element of `value` is already one sample', call. = FALSE)
  if (family == "individual")
    return(.prepare_individual(type, value, sizes, area))
  .prepare_attribute(type, value, sizes, area)
}

.prepare_variable <- function(type, value, sample) {
  if (is.null(sample))
    stop('`sample` is required for type = "', type,
         '": it says which observations belong to the same sample. For a ',
         'series of single observations use type = "xbar.one".',
         call. = FALSE)
  if (length(sample) != length(value))
    stop("`value` (", length(value), ") and `sample` (", length(sample),
         ") must have the same length: one sample label per observation",
         call. = FALSE)
  if (anyNA(value) || anyNA(sample))
    stop("`value` and `sample` must not contain NA", call. = FALSE)
  if (!all(is.finite(value)))
    stop("`value` must not contain Inf or NaN", call. = FALSE)

  # Samples are ordered by FIRST APPEARANCE, which is the only ordering that
  # preserves the time order of a control chart. qcc::qcc.groups() sorts by
  # label instead, so character or out-of-order labels would silently permute
  # the series before change-point detection ever ran; we group by hand.
  lab <- as.character(sample)
  labels <- unique(lab)
  idx    <- match(lab, labels)
  K      <- length(labels)

  # Non-contiguous observations for a sample mean the rows are not in time
  # order; grouping is still well defined, but the caller should know.
  if (any(diff(idx) < 0L))
    warning("the observations of some samples are interleaved in `value`; ",
            "samples were ordered by first appearance. Sort the data by ",
            "time if that is not what you meant.", call. = FALSE)

  ns   <- tabulate(idx, nbins = K)
  nmax <- max(ns)

  if (nmax == 1L)
    stop("all samples are of size 1, so the within-sample sigma of a ",
         type, ' chart is undefined. Use type = "xbar.one" (individuals ',
         "chart), which estimates sigma from the moving range.",
         call. = FALSE)
  if (type == "R" && nmax > .R_MAX_N)
    stop("the R chart is only defined here for samples of at most ",
         .R_MAX_N, " observations (qcc tabulates the d2/d3 constants no ",
         "further), but the largest sample has ", nmax,
         '. Use type = "S", which has a closed form for any sample size.',
         call. = FALSE)
  if (length(unique(ns)) > 1L)
    warning("samples have unequal sizes (", min(ns), " to ", nmax,
            "); shorter samples are NA-padded and their limits vary ",
            "accordingly", call. = FALSE)

  # Rows = samples in time order, columns = observations within the sample.
  pos <- stats::ave(seq_along(idx), idx, FUN = seq_along)
  groups <- matrix(NA_real_, nrow = K, ncol = nmax,
                   dimnames = list(labels, NULL))
  groups[cbind(idx, pos)] <- value

  list(type = type, family = "variable", K = K, groups = groups,
       value = NULL, sizes = NULL, area = NULL, labels = labels,
       sample_sizes = ns)
}

.prepare_individual <- function(type, value, sizes, area) {
  if (!is.null(sizes) || !is.null(area))
    stop('`sizes` and `area` do not apply to type = "', type,
         '"; every sample is a single observation', call. = FALSE)
  if (anyNA(value) || !all(is.finite(value)))
    stop("`value` must not contain NA, Inf or NaN", call. = FALSE)
  K <- length(value)
  list(type = type, family = "individual", K = K, groups = NULL,
       value = value, sizes = NULL, area = NULL,
       labels = as.character(seq_len(K)), sample_sizes = rep(1L, K))
}

.prepare_attribute <- function(type, value, sizes, area) {
  K <- length(value)
  if (type %in% c("p", "np")) {
    if (!is.null(area))
      stop('`area` applies only to the u chart, not to type = "', type, '"',
           call. = FALSE)
    if (is.null(sizes))
      stop('`sizes` is required for type = "', type,
           '": the number inspected in each sample', call. = FALSE)
    sizes <- .check_counts(sizes, "sizes", K = K, positive = TRUE,
                           what = "size")
    value <- .check_counts(value, "value", what = "count")
    if (any(value > sizes))
      stop("`value` counts defectives, so it cannot exceed `sizes`; ",
           sum(value > sizes), " sample(s) do (first at sample ",
           which(value > sizes)[1L], ": ", value[which(value > sizes)[1L]],
           " of ", sizes[which(value > sizes)[1L]], ")", call. = FALSE)
    area <- NULL
  } else if (type == "u") {
    if (!is.null(sizes))
      stop('use `area` rather than `sizes` for type = "u" (the inspection ',
           "area or number of units per sample)", call. = FALSE)
    if (is.null(area))
      stop('`area` is required for type = "u": the inspection area of each ',
           "sample", call. = FALSE)
    area  <- .check_counts(area, "area", K = K, positive = TRUE,
                           whole = FALSE, what = "area")
    value <- .check_counts(value, "value", what = "count")
    sizes <- NULL
  } else { # c
    if (!is.null(sizes))
      stop('`sizes` does not apply to type = "c"; it assumes a constant ',
           "inspection unit, so `value` alone holds the defects per sample. ",
           'Use type = "u" with `area` for varying inspection size.',
           call. = FALSE)
    if (!is.null(area))
      stop('`area` applies only to the u chart; for a varying inspection ',
           'size use type = "u"', call. = FALSE)
    value <- .check_counts(value, "value", what = "count")
    sizes <- NULL; area <- NULL
  }
  list(type = type, family = "attribute", K = K, groups = NULL,
       value = value, sizes = sizes, area = area,
       labels = as.character(seq_len(K)), sample_sizes = rep(1L, K))
}

# segqcc 0.1.0

First release.

* `segmented_qcc()` detects change points in a control-chart statistic and
  rebuilds the chart with the centre and the control limits recomputed within
  each homogeneous segment.
* Chart types: `"xbar"`, `"R"`, `"S"` (grouped samples), `"xbar.one"`
  (individuals) and `"p"`, `"np"`, `"c"`, `"u"` (attributes).
* `segmented_xbar()` is a shorthand for the X-bar case.
* `plot_segmented_qcc()`, and `plot()`, `print()` and `summary()` methods for
  the `segmented_qcc` class.
* The charted statistic is standardised before detection (`scale`), which makes
  detection invariant to the units of the data. `changepoint`'s normal cost
  assumes unit variance, so without this a series with a standard deviation of
  20 draws upwards of 160 spurious change points where there are none.
* Series that cannot be segmented — too short, constant, `min_seg_len` larger
  than the series, or a detector that refuses the data — return an ordinary
  single-segment chart with a warning and a reason in `$notes`, rather than an
  error. A segment that cannot be refitted on its own falls back to the limits
  of the whole series.
* Samples are ordered by first appearance, so arbitrary sample labels never
  permute the series.
* Bundled data sets: `segxbar`, `segind`, `segp`, `segc`.

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
* Run-rule violations are reported alongside the samples beyond the limits, in
  `$violating_runs` and the `n_runs` column, and drawn in orange as `qcc` does.
  `qcc` counts runs over the whole series; `segmented_qcc()` counts them within
  each segment, since a run straddling a change point measures its samples
  against two different centres. Controlled by `run_length`.
* A segment of one or two samples is warned about: its limits are estimated
  from the very samples they judge, so nothing in it can come out as out of
  control, and fitting a segment around an isolated outlier hides it rather
  than flagging it. Raising `min_seg_len` is the fix.
* Fixed: a p, np or u chart whose sample sizes vary was judged against a single
  pair of limits per segment, taken from that segment's first sample. The limits
  of these charts depend on the size of each sample, so most of the series was
  compared against a band that never applied to it - on a 200-sample p chart
  with sizes between 40 and 120, two samples were reported out of control where
  none are. Each sample now carries its own limits, which the plot draws as a
  step and the `segments` table reports as a range (`LCL`..`LCL_max`,
  `UCL_min`..`UCL`, flagged by `limits_vary`).
* Count charts are routed to `changepoint`'s native Poisson cost rather than a
  normal one, whose assumptions a series of small counts does not meet. On a
  homogeneous series with a mean of 0.2 the normal cost reports about 3 change
  points that are not there and the Poisson cost none; a rise from 0.3 to 1 is
  found 83% of the time rather than 52%. From a mean of about 2 upwards the two
  agree. Controlled by `test_stat`; `p` and `np` keep the standardised normal
  cost, since `changepoint` offers no binomial one.
* `capability_by_segment()` and `plot_capability_by_segment()` compute
  capability indices within each segment, flagging segments that are not in
  control.
* Bundled data sets: `segxbar`, `segind`, `segp`, `segc`.

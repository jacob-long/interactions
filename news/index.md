# Changelog

## interactions 1.3.0

- Fixed
  [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  refits silently discarding weights and offsets. Ordinary `lm`/`glm`
  refits now preserve the estimation sample, evaluated weights, and
  total offset, without reapplying subsets or reevaluating these
  expressions on centered variables. Original-data recovery checks row
  alignment and model frame agreement; explicit `data` is honored for
  these refits.

- Fixed non-survey simple-slopes `at` values being applied after
  mean-centering. They now override centering on the original scale, as
  documented.

- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  and
  [`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md)
  are now S3 generics. New panelr methods use design-matrix contrasts
  and the fitted covariance matrix without refits, with the same
  within/original-scale conventions as interaction plots.
  [`probe_interaction()`](https://interactions.jacob-long.com/reference/probe_interaction.md),
  printing, tidying, coefficient plots, and tables are supported. Other
  model classes retain their existing calculation path.

- Panelr Johnson-Neyman results explicitly represent zero, one, or two
  crossing points, including all-significant and nowhere-significant
  cases. Tests are conditional, fixed-effect, and link-scale, with a
  stated degrees of freedom approximation; unsupported inference options
  fail explicitly.

- Added component-aware panelr interaction plots. Numeric within-person
  terms default to deviations in original units, including ranges and
  moderator values; constant terms retain their original units or
  categories. Labels and captions explain the scale and reference means.
  `plot.scale = "original"` selects raw levels, and out-of-range
  deviations warn about extrapolation. Requires panelr 1.1.0 or later.
  Other model classes are unchanged.

- Requires jtools 2.3.2 or later for the refitting and compatibility
  fixes.

## interactions 1.2.0

CRAN release: 2024-07-29

A special thanks to Github user [@olivroy](https://github.com/olivroy)
for contributing a number of mostly under-the-hood updates prior to this
release.

Enhancements:

- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now supports non-continuous variables in the `pred` argument.
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now has an `at` argument, allowing you to specify an exact, perhaps
  non-centered, level for variables not involved in the interaction.
- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  now has provisional support for factor predictors (`pred`). Users will
  receive a message because of the possibility for unexpected behavior.
  [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  likewise has support for continuous moderators.
  ([\#54](https://github.com/jacob-long/interactions/issues/54))
- Website and some documentation have been revamped and upgraded.
- Users can now change the axis labels for
  [`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md)
  plots via the arguments `y.label` and `modx.label`.
  ([\#56](https://github.com/jacob-long/interactions/issues/56))
- Models produced by the `panelr` package are better supported.

Bug fixes:

- [`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md)
  now handles non-syntactic variable names for `modx` correctly.
  ([\#56](https://github.com/jacob-long/interactions/issues/56))
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  no longer displays results with factor moderators in the reverse order
  of the factor’s levels.
  ([\#55](https://github.com/jacob-long/interactions/issues/55))
- `probe_interactions()` no longer errors when certain combinations of
  arguments are provided.
  ([\#50](https://github.com/jacob-long/interactions/issues/50))
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  no longer errors when ordered factors are moderators. Thanks to
  Jonathan Zadra for suggesting the fix.
  ([\#42](https://github.com/jacob-long/interactions/issues/42))
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  no longer errors when given `merModTest` objects.

## interactions 1.1.5

CRAN release: 2021-07-02

- Made a small change to avoid testing errors in a forthcoming R
  release.

## interactions 1.1.4

Bugfix:

- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now correctly handles the `robust` argument when it is not set to
  `TRUE` or `FALSE`. Many thanks to Andy Field for reporting the issue.
  ([\#36](https://github.com/jacob-long/interactions/issues/36))

## interactions 1.1.3

CRAN release: 2020-04-04

Minor fix:

- Plotting functions no longer fail when there is missing data in the
  moderator variable(s).

## interactions 1.1.2

Bugfixes:

- Plotting functions no longer fail with incomplete source data.
- Plotting functions now respect the order of `modx.values` and
  `mod2.values` arguments.
  ([\#29](https://github.com/jacob-long/interactions/issues/29))
- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  no longer ignores the `point.alpha` argument.
  ([\#25](https://github.com/jacob-long/interactions/issues/25))
- Plotting functions now allow you to specify labels for `modx.values`
  or `mod2.values` by passing a named vector to those arguments.
  ([\#30](https://github.com/jacob-long/interactions/issues/30))
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now prints labels when requested with the `modx.labels` or
  `mod2.labels` arguments.
  ([\#32](https://github.com/jacob-long/interactions/issues/32))

Feature update:

- Plotting functions now better support `brmsfit` objects, in particular
  those with multiple dependent variables and distributional dependent
  variables. Use the `resp` and `dpar` arguments to specify which you
  want to use.

## interactions 1.1.1

CRAN release: 2019-07-05

Bugfixes:

- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  no longer fails getting Johnson-Neyman intervals for `merMod` models.
  ([\#20](https://github.com/jacob-long/interactions/issues/20))
- [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  no longer ignores `pred.values` and `pred.labels` arguments. Thanks to
  [Paul Djupe](http://pauldjupe.com/) for alerting me to this.
- The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method
  for `sim_slopes` objects no longer returns numbers as strings. This
  had downstream effects on, e.g., the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
  `sim_slopes`.
  ([\#22](https://github.com/jacob-long/interactions/issues/22); thanks
  to Noah Greifer)
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now handles `lmerModTest` objects properly. Thanks to Eric Shuman for
  bringing it to my attention.

## interactions 1.1.0

CRAN release: 2019-04-28

### New function: `sim_margins()`

This is, as the name suggests, related to
[`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md).
However, instead of *slopes*, what is being estimated are [marginal
effects](https://cran.r-project.org/package=margins/vignettes/TechnicalDetails.pdf).
In the case of OLS linear regression, this is basically the same thing.
The slope in OLS is the expected change in the outcome for each 1-unit
increase in the predictor. For other models, however, the actual change
in the outcome when there’s a 1-unit increase in a variable depends on
the level of other covariates and the initial value of the predictor. In
a logit model, for instance, the change in probability will be different
if the initial probability was 50% (could go quite a bit up or down)
than if it was 99.9% (can’t go up).

[`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md)
uses the [`margins`](https://cran.r-project.org/package=margins) package
under the hood to estimate marginal effects. Unlike
[`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md),
in which by default all covariates not involved in the interaction are
mean-centered, in
[`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md)
these covariates are always left at their observed values because they
influence the level of the marginal effect. Instead, the marginal effect
is calculated with the covariates and focal predictor (`pred`) at their
observed values and the moderator(s) held at the specified values (e.g.,
the mean and 1 standard deviation above/below the mean). I advise using
[`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md)
rather than
[`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
when analyzing models other than OLS regression.

### Bug fixes

- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  and
  [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  now respect the user’s selection of `outcome.scale`; in 1.0.0, it
  always plotted on the response scale.
  ([\#12](https://github.com/jacob-long/interactions/issues/12))
- The `modx.values` argument is now better documented to explain that
  you may use it to specify the exact values you want. Thanks to Jakub
  Lysek for asking the question that prompted this.
  ([\#8](https://github.com/jacob-long/interactions/issues/8))
- `modx.values` now accepts `"mean-plus-minus"` as a manual
  specification of the default auto-calculated values for continuous
  moderators. `NULL` still defaults to this, but you can now make this
  explicit in your code if desired for clarity or to guard against
  future changes in the default behavior.
- Users are now warned when `modx.values` or `mod2.values` include
  values outside the observed range of the `modx`/`mod2`.
  ([\#9](https://github.com/jacob-long/interactions/issues/9))
- Users are now warned when `pred`, `modx`, and `mod2` are not all
  involved in an interaction with each other in the provided model.
  ([\#10](https://github.com/jacob-long/interactions/issues/10))
- [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  was ignoring `mod2.values` arguments but now works properly.
  ([\#17](https://github.com/jacob-long/interactions/issues/17))
- Missing values in the original data are now handled better in
  [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  and
  [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md).
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now handles non-syntactic variable names better.
- `interactions` now requires you to have a relatively new version of
  `rlang`. Users with older versions were experiencing cryptic errors.
  ([\#15](https://github.com/jacob-long/interactions/issues/15))

### Feature updates

- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  and
  [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  now have an `at` argument for more granular control over the values of
  covariates.
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  now allows for custom specification of robust standard error
  estimators via providing a function to `v.cov` and arguments to
  `v.cov.args`.

## interactions 1.0.0

CRAN release: 2019-02-18

This is the first release, but a look at the NEWS for
[`jtools`](https://jtools.jacob-long.com) prior to its version 2.0.0
will give you an idea of the history of the functions in this package.

What follows is an accounting of changes to functions in this package
since they were last in `jtools`.

- Plots made by `interactions` now have a new theme, which you can use
  yourself, called
  [`theme_nice()`](https://jtools.jacob-long.com/reference/theme_nice.html)
  (from the `jtools` package). The previous default,
  [`theme_apa()`](https://jtools.jacob-long.com/reference/theme_apa.html),
  is still available but I don’t like it as a default since I don’t
  think the APA has defined the nicest-looking design guidelines for
  general use.
- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  now has appropriate coloring for observed data when the moderator is
  numeric ([\#1](https://github.com/jacob-long/interactions/issues/1)).
  In previous versions I had to use a workaround that involved tweaking
  the alpha of the observed data points.
- [`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
  and
  [`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
  now use *tidy evaluation* for the `pred`, `modx`, and `mod2`
  arguments. This means you can pass a variable that contains the name
  of `pred`/`modx`/`mod2`, which is most useful if you are creating a
  function, for loop, etc. If using a variable, put a `!!` from the
  `rlang` package before it (e.g., `pred = !! variable`). For most
  users, these changes will not affect their usage.
- [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  no longer prints coefficient tables as data frames because this caused
  RStudio notebook users issues with the output not being printed to the
  console and having the notebook format them in less-than-ideal ways.
  The tables now have a markdown format that might remind you of Stata’s
  coefficient tables. Thanks to Kim Henry for contacting me about this.

### Use partial residuals for plotting

One negative when visualizing predictions alongside original data with
[`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
or similar tools is that the observed data may be too spread out to pick
up on any patterns. However, sometimes your model is controlling for the
causes of this scattering, especially with multilevel models that have
random intercepts. Partial residuals include the effects of all the
controlled-for variables and let you see how well your model performs
with all of those things accounted for.

You can plot partial residuals instead of the observed data in
[`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md)
and
[`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md)
via the argument `partial.residuals = TRUE`.

### Important changes to `make_predictions()` and removal of `plot_predictions()`

In the `jtools` 1.0.0 release, I introduced
[`make_predictions()`](https://jtools.jacob-long.com/reference/make_predictions.html)
as a lower-level way to emulate the functionality of
[`effect_plot()`](https://jtools.jacob-long.com/reference/effect_plot.html),
[`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md),
and
[`cat_plot()`](https://interactions.jacob-long.com/reference/cat_plot.md).
This would return a list object with predicted data, the original data,
and a bunch of attributes containing information about how to plot it.
One could then take this object, with class `predictions`, and use it as
the main argument to `plot_predictions()`, which was another new
function that creates the plots you would see in
[`effect_plot()`](https://jtools.jacob-long.com/reference/effect_plot.html)
et al.

I have simplified
[`make_predictions()`](https://jtools.jacob-long.com/reference/make_predictions.html)
to be less specific to those plotting functions and eliminated
`plot_predictions()`, which was ultimately too complex to maintain and
caused problems for separating the interaction tools into a separate
package.
[`make_predictions()`](https://jtools.jacob-long.com/reference/make_predictions.html)
by default simply creates a new data frame of predicted values along a
`pred` variable. It no longer accepts `modx` or `mod2` arguments.
Instead, it accepts an argument called `at` where a user can specify any
number of variables and values to generate predictions *at*. This syntax
is designed to be similar to the `predictions`/`margins` packages. See
the `jtools` documentation for more info on this revised syntax.

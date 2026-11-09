# Perform a simple slopes analysis.

`sim_slopes` conducts a simple slopes analysis for the purposes of
understanding two- and three-way interaction effects in linear
regression.

## Usage

``` r
sim_slopes(
  model,
  pred,
  modx,
  mod2 = NULL,
  modx.values = NULL,
  mod2.values = NULL,
  centered = "all",
  at = NULL,
  data = NULL,
  cond.int = FALSE,
  johnson_neyman = TRUE,
  jnplot = FALSE,
  jnalpha = 0.05,
  robust = FALSE,
  digits = getOption("jtools-digits", default = 2),
  pvals = TRUE,
  confint = FALSE,
  ci.width = 0.95,
  cluster = NULL,
  modx.labels = NULL,
  mod2.labels = NULL,
  v.cov = NULL,
  v.cov.args = NULL,
  plot.scale = c("within", "original"),
  df = NULL,
  ...
)
```

## Arguments

- model:

  A regression model. The function is tested with `lm`, `glm`,
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html),
  [`merMod`](https://rdrr.io/pkg/lme4/man/merMod-class.html), `rq` (from
  `quantreg`),
  [`brmsfit`](https://paulbuerkner.com/brms/reference/brmsfit-class.html),
  `stanreg` models. Models from other classes may work as well but are
  not officially supported. The model should include the interaction of
  interest.

- pred:

  The name of the predictor variable involved in the interaction. This
  can be a bare name or string. Note that it is evaluated using `rlang`,
  so programmers can use the `!!` syntax to pass variables instead of
  the verbatim names.

- modx:

  The name of the moderator variable involved in the interaction. This
  can be a bare name or string. The same `rlang` proviso applies as with
  `pred`.

- mod2:

  Optional. The name of the second moderator variable involved in the
  interaction. This can be a bare name or string. The same `rlang`
  proviso applies as with `pred`.

- modx.values:

  For which values of the moderator should lines be plotted? There are
  two basic options:

  - A vector of values (e.g., `c(1, 2, 3)`)

  - A single argument asking to calculate a set of values. See details
    below.

  Default is `NULL`. If `NULL` (or `mean-plus-minus`), then the
  customary +/- 1 standard deviation from the mean as well as the mean
  itself are used for continuous moderators. If `"plus-minus"`, plots
  lines when the moderator is at +/- 1 standard deviation without the
  mean. You may also choose `"terciles"` to split the data into
  equally-sized groups and choose the point at the mean of each of those
  groups.

  If the moderator is a factor variable and `modx.values` is `NULL`,
  each level of the factor is included. You may specify any subset of
  the factor levels (e.g., `c("Level 1", "Level 3")`) as long as there
  is more than 1. The levels will be plotted in the order you provide
  them, so this can be used to reorder levels as well.

- mod2.values:

  For which values of the second moderator should the plot be facetted
  by? That is, there will be a separate plot for each level of this
  moderator. Defaults are the same as `modx.values`.

- centered:

  A vector of quoted variable names that are to be mean-centered. If
  `"all"`, all non-focal predictors as well as the `pred` variable are
  centered. You may instead pass a character vector of variables to
  center. User can also use "none" to base all predictions on variables
  set at 0. The response variable, `modx`, and `mod2` variables are
  never centered.

- at:

  If you want to manually set the values of other variables in the
  model, do so by providing a named list where the names are the
  variables and the list values are vectors of the values. Note that you
  cannot alter the values of the `pred`, `modx`, or `mod2` variables and
  this will take precedence over the `centered` argument (but any
  variables unmentioned by `at` will be centered as specified by
  `centered`). For linear models, this will only change the output of
  the conditional intercepts.

- data:

  Optional, default is NULL. You may provide the data used to fit the
  model. This can be a better way to get mean values for centering and
  can be crucial for models with variable transformations in the formula
  (e.g., `log(x)`) or polynomial terms (e.g., `poly(x, 2)`). You will
  see a warning if the function detects problems that would likely be
  solved by providing the data with this argument and the function will
  attempt to retrieve the original data from the global environment.

- cond.int:

  Should conditional intercepts be printed in addition to the slopes?
  Default is `FALSE`.

- johnson_neyman:

  Should the Johnson-Neyman interval be calculated? Default is `TRUE`.
  This can be performed separately with
  [`johnson_neyman`](https://interactions.jacob-long.com/reference/johnson_neyman.md).

- jnplot:

  Should the Johnson-Neyman interval be plotted as well? Default is
  `FALSE`.

- jnalpha:

  What should the alpha level be for the Johnson-Neyman interval?
  Default is .05, which corresponds to a 95% confidence interval.

- robust:

  Should robust standard errors be used to find confidence intervals for
  supported models? Default is FALSE, but you should specify the type of
  sandwich standard errors if you'd like to use them (i.e., `"HC0"`,
  `"HC1"`, and so on). If `TRUE`, defaults to `"HC3"` standard errors.

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 2. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- pvals:

  Show p values? If `FALSE`, these are not printed. Default is `TRUE`.

- confint:

  Show confidence intervals instead of standard errors? Default is
  `FALSE`.

- ci.width:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is `.95`, which corresponds to a 95%
  confidence interval. Ignored if `confint = FALSE`.

- cluster:

  For clustered standard errors, provide the column name of the cluster
  variable in the input data frame (as a string). Alternately, provide a
  vector of clusters.

- modx.labels:

  A character vector of labels for each level of the moderator values,
  provided in the same order as the `modx.values` argument. If `NULL`,
  the values themselves are used as labels unless `modx,values` is also
  `NULL`. In that case, "+1 SD" and "-1 SD" are used.

- mod2.labels:

  A character vector of labels for each level of the 2nd moderator
  values, provided in the same order as the `mod2.values` argument. If
  `NULL`, the values themselves are used as labels unless `mod2.values`
  is also `NULL`. In that case, "+1 SD" and "-1 SD" are used.

- v.cov:

  A function to calculate variances for the model. Examples could be
  [`sandwich::vcovPC()`](https://zeileis.codeberg.page/sandwich/reference/vcovPC.html).

- v.cov.args:

  A list of arguments for the `v.cov` function. For whichever argument
  should be the fitted model, put `"model"`.

- plot.scale:

  For panelr within-between models, `"within"` (default) displays
  numeric within-person predictors as deviations from each person's
  observed mean, in original units. Default ranges and moderator values
  use within-person variation. Constant moderators retain their original
  scale. Use `"original"` for raw predictor levels conditional on
  reference person means. Numeric moderator values and `at` use the
  selected display scale; `imean(...)` overrides always use original
  units. Axes, legends, and captions identify the interpretation. Values
  outside observed within-person ranges produce warnings; these checks
  do not establish joint support. Requires a panelr version exporting
  `prepare_wb_plot()`. Ignored for other model classes. See panelr's
  prediction documentation for limitations.

- df:

  For panelr models, `NULL` uses a residual-df t approximation for
  Gaussian mixed models and a normal approximation for generalized mixed
  models and GEE. Alternatively use `"residual"`, `"normal"`, or a
  positive number. These are not Satterthwaite or Kenward-Roger tests.
  For other model classes an explicitly supplied value is forwarded to
  the default method.

- ...:

  Arguments passed to
  [`johnson_neyman`](https://interactions.jacob-long.com/reference/johnson_neyman.md)
  and `summ`.

## Value

A list object with the following components:

- slopes:

  A table of coefficients for the focal predictor at each value of the
  moderator

- ints:

  A table of coefficients for the intercept at each value of the
  moderator

- modx.values:

  The values of the moderator used in the analysis

- mods:

  A list containing each regression model created to estimate the
  conditional coefficients. Empty for contrast-based panelr methods.

- model:

  For panelr methods, the original fitted model; it is not modified.

- contrasts:

  For panelr methods, the slope contrast matrices used in the
  calculation, one per second-moderator value.

- jn:

  If `johnson_neyman = TRUE`, a list of `johnson_neyman` objects from
  [`johnson_neyman`](https://interactions.jacob-long.com/reference/johnson_neyman.md).
  These contain the values of the interval and the plots. If a 2-way
  interaction, the list will be of length

  1.  Otherwise, there will be 1 `johnson_neyman` object for each value
      of the 2nd moderator for 3-way interactions.

## Details

This allows the user to perform a simple slopes analysis for the purpose
of probing interaction effects in a linear regression. Two- and
three-way interactions are supported, though one should be warned that
three-way interactions are not easy to interpret in this way.

For more about Johnson-Neyman intervals, see
[`johnson_neyman`](https://interactions.jacob-long.com/reference/johnson_neyman.md).

The function is tested with `lm`, `glm`, `svyglm`, and `merMod` inputs.
Others may work as well, but are not tested. In all but the linear model
case, be aware that not all the assumptions applied to simple slopes
analysis apply.

## Ordinary lm and glm refits

Refits retain the original estimation sample, fitted weights, and total
offset. Subset expressions are not reapplied, and weight/offset
expressions are not reevaluated after centering moderators. Conditional
intercepts are regression intercepts with the known offset contribution
excluded. When original variables must be recovered from `data`, provide
unchanged fitting data with its original row names. The recovered model
frame is checked against the fitted model before refitting.

## Panelr models

With a panelr version providing `wb_slope_design()`, `wbm` and `wbgee`
models use exact design-matrix contrasts rather than shifted-data
refits. If `L` is the difference between design rows one focal-predictor
unit apart, the slope is `L %*% beta` and its variance is
`L %*% vcov(model) %*% t(L)`. All covariance terms are retained. Other
model classes retain the existing refit method.

Numeric focal predictors, numeric/factor moderators, and two- and
three-way interactions are supported. `plot.scale = "within"` uses the
same deviations, moderator defaults, and fixed reference quantities as
[`interact_plot()`](https://interactions.jacob-long.com/reference/interact_plot.md).
Constant moderators retain original units/categories. `"original"` uses
raw levels. Slopes use original predictor units even for standardized
fits, and the outcome's fitted scale. Intercepts refer to a focal
predictor of zero on the selected display scale. Offset variables absent
from the formula must be supplied explicitly in `at`, in the same way as
for prediction grids.

Slopes are fixed-effect slopes conditional on reference person means and
interaction-centering quantities, not derivatives of recomputed panel
histories or averages over random effects. For non-identity links they
are link-scale slopes, not response-scale marginal effects. The fitted
GEE sandwich covariance is retained. Custom covariance estimators,
FDR-adjusted Johnson-Neyman inference, nonlinear focal terms,
predictor-dependent offsets, factor focal predictors, rank-deficient
fits, and the plotting exclusions for lagged/detrended/basis models are
not supported and produce errors.

Johnson-Neyman regions use the same contrasts, covariance matrix, and
degrees of freedom as the slope tests. Their intervals are pointwise,
not simultaneous confidence bands.
[`probe_interaction()`](https://interactions.jacob-long.com/reference/probe_interaction.md)
shares the display scale and moderator settings between the slope and
plotting methods.

## References

Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
multilevel regression: Inferential and graphical techniques.
*Multivariate Behavioral Research*, *40*(3), 373-400.
[doi:10.1207/s15327906mbr4003_5](https://doi.org/10.1207/s15327906mbr4003_5)

Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied
multiple regression/correlation analyses for the behavioral sciences*
(3rd ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.

## See also

[`interact_plot`](https://interactions.jacob-long.com/reference/interact_plot.md)
accepts similar syntax and will plot the results with
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

`testSlopes()` from `rockchalk` performs a hypothesis test of
differences and provides Johnson-Neyman intervals.

`simpleSlope()` from `pequod` performs a similar analysis.

Other interaction tools:
[`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md),
[`probe_interaction()`](https://interactions.jacob-long.com/reference/probe_interaction.md),
[`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r

# Using a fitted model as formula input
fiti <- lm(Income ~ Frost + Murder * Illiteracy,
  data = as.data.frame(state.x77))
sim_slopes(model = fiti, pred = Murder, modx = Illiteracy)
#> JOHNSON-NEYMAN INTERVAL
#> 
#> When Illiteracy is OUTSIDE the interval [0.68, 2.20], the slope of
#> Murder is p < .05.
#> 
#> Note: The range of observed values of Illiteracy is [0.50, 2.80]
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of Murder when Illiteracy = 0.5604669 (- 1 SD): 
#> 
#>    Est.    S.E.   t val.      p
#> ------- ------- -------- ------
#>   78.99   34.81     2.27   0.03
#> 
#> Slope of Murder when Illiteracy = 1.1700000 (Mean): 
#> 
#>   Est.    S.E.   t val.      p
#> ------ ------- -------- ------
#>   8.36   29.24     0.29   0.78
#> 
#> Slope of Murder when Illiteracy = 1.7795331 (+ 1 SD): 
#> 
#>     Est.    S.E.   t val.      p
#> -------- ------- -------- ------
#>   -62.28   41.49    -1.50   0.14
#> 

# With svyglm
if (requireNamespace("survey")) {
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc = ~fpc)
regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
sim_slopes(regmodel, pred = ell, modx = meals)

# 3-way with survey and factor input
regmodel <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
sim_slopes(regmodel, pred = ell, modx = meals, mod2 = sch.wide)
}
#> ██████████████████████ While sch.wide (2nd moderator) = No █████████████████████ 
#> 
#> JOHNSON-NEYMAN INTERVAL
#> 
#> The Johnson-Neyman interval could not be found. Is the p value for your
#> interaction term below the specified alpha?
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of ell when meals = 18.72139 (- 1 SD): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -3.51   1.99    -1.76   0.08
#> 
#> Slope of ell when meals = 48.22427 (Mean): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -1.98   1.25    -1.58   0.11
#> 
#> Slope of ell when meals = 77.72716 (+ 1 SD): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.45   1.23    -0.37   0.71
#> 
#> █████████████████████ While sch.wide (2nd moderator) = Yes █████████████████████ 
#> 
#> JOHNSON-NEYMAN INTERVAL
#> 
#> The Johnson-Neyman interval could not be found. Is the p value for your
#> interaction term below the specified alpha?
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of ell when meals = 18.72139 (- 1 SD): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.93   0.83    -1.12   0.26
#> 
#> Slope of ell when meals = 48.22427 (Mean): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.62   0.56    -1.12   0.26
#> 
#> Slope of ell when meals = 77.72716 (+ 1 SD): 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.32   0.38    -0.84   0.40
#> 
```

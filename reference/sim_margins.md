# Perform a simple margins analysis.

`sim_margins` conducts a simple margins analysis for the purposes of
understanding two- and three-way interaction effects in linear
regression.

## Usage

``` r
sim_margins(
  model,
  pred,
  modx,
  mod2 = NULL,
  modx.values = NULL,
  mod2.values = NULL,
  data = NULL,
  cond.int = FALSE,
  vce = c("delta", "simulation", "bootstrap", "none"),
  iterations = 1000,
  digits = getOption("jtools-digits", default = 2),
  pvals = TRUE,
  confint = FALSE,
  ci.width = 0.95,
  cluster = NULL,
  modx.labels = NULL,
  mod2.labels = NULL,
  ...
)
```

## Arguments

- model:

  A regression model. The function is tested with `lm`, `glm`,
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html),
  [`merMod`](https://rdrr.io/pkg/lme4/man/merMod-class.html), `rq`,
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

- vce:

  A character string indicating the type of estimation procedure to use
  for estimating variances. The default (“delta”) uses the delta method.
  Alternatives are “bootstrap”, which uses bootstrap estimation, or
  “simulation”, which averages across simulations drawn from the joint
  sampling distribution of model coefficients. The latter two are
  extremely time intensive.

- iterations:

  If `vce = "bootstrap"`, the number of bootstrap iterations. If
  `vce = "simulation"`, the number of simulated effects to draw. Ignored
  otherwise.

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

- ...:

  ignored.

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

## Details

This allows the user to perform a simple margins analysis for the
purpose of probing interaction effects in a linear regression. Two- and
three-way interactions are supported, though one should be warned that
three-way interactions are not easy to interpret in this way.

The function is tested with `lm`, `glm`, `svyglm`, and `merMod` inputs.
Others may work as well, but are not tested. In all but the linear model
case, be aware that not all the assumptions applied to simple slopes
analysis apply.

## References

Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
multilevel regression: Inferential and graphical techniques.
*Multivariate Behavioral Research*, *40*(3), 373-400.
[doi:10.1207/s15327906mbr4003_5](https://doi.org/10.1207/s15327906mbr4003_5)

Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied
multiple regression/correlation analyses for the behavioral sciences*
(3rd ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.

Hanmer, M. J., & Kalkan, K. O. (2013). Behind the curve: Clarifying the
best approach to calculating predicted probabilities and marginal
effects from limited dependent variable models. *American Journal of
Political Science*, *57*, 263–277.
[doi:10.1111/j.1540-5907.2012.00602.x](https://doi.org/10.1111/j.1540-5907.2012.00602.x)

## See also

[`margins::margins()`](https://rdrr.io/pkg/margins/man/margins.html)

Other interaction tools:
[`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md),
[`probe_interaction()`](https://interactions.jacob-long.com/reference/probe_interaction.md),
[`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)

## Author

Jacob Long <jacob.long@sc.edu>

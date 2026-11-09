# Probe interaction effects via simple slopes and plotting

`probe_interaction` is a convenience function that allows users to call
both
[`sim_slopes`](https://interactions.jacob-long.com/reference/sim_slopes.md)
and
[`interact_plot`](https://interactions.jacob-long.com/reference/interact_plot.md)
with a single call.

## Usage

``` r
probe_interaction(model, pred, modx, mod2 = NULL, ...)
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

- ...:

  Other arguments accepted by
  [`sim_slopes`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  and
  [`interact_plot`](https://interactions.jacob-long.com/reference/interact_plot.md)

## Value

- simslopes:

  The `sim_slopes` object created.

- interactplot:

  The `ggplot` object created by `interact_plot`.

## Details

This function simply merges the nearly-equivalent arguments needed to
call both
[`sim_slopes`](https://interactions.jacob-long.com/reference/sim_slopes.md)
and
[`interact_plot`](https://interactions.jacob-long.com/reference/interact_plot.md)
without the need for re-typing their common arguments. Note that each
function is called separately and they re-fit a separate model for each
level of each moderator; therefore, the runtime may be considerably
longer than the original model fit. For larger models, this is worth
keeping in mind.

Sometimes, you may want different parameters when doing simple slopes
analysis compared to when plotting interaction effects. For instance, it
is often easier to interpret the regression output when variables are
standardized; but plots are often easier to understand when the
variables are in their original units of measure.

`probe_interaction` does not support providing different arguments to
each function. If that is needed, use `sim_slopes` and `interact_plot`
directly.

## See also

Other interaction tools:
[`johnson_neyman()`](https://interactions.jacob-long.com/reference/johnson_neyman.md),
[`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md),
[`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r

# Using a fitted model as formula input
fiti <- lm(Income ~ Frost + Murder * Illiteracy,
  data=as.data.frame(state.x77))
probe_interaction(model = fiti, pred = Murder, modx = Illiteracy,
                  modx.values = "plus-minus")
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
#> Slope of Murder when Illiteracy = 1.7795331 (+ 1 SD): 
#> 
#>     Est.    S.E.   t val.      p
#> -------- ------- -------- ------
#>   -62.28   41.49    -1.50   0.14
#> 

# 3-way interaction
fiti3 <- lm(Income ~ Frost * Murder * Illiteracy,
  data=as.data.frame(state.x77))
probe_interaction(model = fiti3, pred = Murder, modx = Illiteracy,
                  mod2 = Frost, mod2.values = "plus-minus")
#> ███████████████ While Frost (2nd moderator) =  52.47915 (- 1 SD) ███████████████ 
#> 
#> JOHNSON-NEYMAN INTERVAL
#> 
#> The Johnson-Neyman interval could not be found. Is the p value for your
#> interaction term below the specified alpha?
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of Murder when Illiteracy = 0.5604669 (- 1 SD): 
#> 
#>    Est.    S.E.   t val.      p
#> ------- ------- -------- ------
#>   66.10   71.68     0.92   0.36
#> 
#> Slope of Murder when Illiteracy = 1.1700000 (Mean): 
#> 
#>   Est.    S.E.   t val.      p
#> ------ ------- -------- ------
#>   2.73   49.65     0.06   0.96
#> 
#> Slope of Murder when Illiteracy = 1.7795331 (+ 1 SD): 
#> 
#>     Est.    S.E.   t val.      p
#> -------- ------- -------- ------
#>   -60.63   48.83    -1.24   0.22
#> 
#> ███████████████ While Frost (2nd moderator) = 156.44085 (+ 1 SD) ███████████████ 
#> 
#> JOHNSON-NEYMAN INTERVAL
#> 
#> The Johnson-Neyman interval could not be found. Is the p value for your
#> interaction term below the specified alpha?
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of Murder when Illiteracy = 0.5604669 (- 1 SD): 
#> 
#>    Est.    S.E.   t val.      p
#> ------- ------- -------- ------
#>   82.37   42.28     1.95   0.06
#> 
#> Slope of Murder when Illiteracy = 1.1700000 (Mean): 
#> 
#>   Est.    S.E.   t val.      p
#> ------ ------- -------- ------
#>   3.15   49.60     0.06   0.95
#> 
#> Slope of Murder when Illiteracy = 1.7795331 (+ 1 SD): 
#> 
#>     Est.    S.E.   t val.      p
#> -------- ------- -------- ------
#>   -76.07   98.92    -0.77   0.45
#> 


# With svyglm
if (requireNamespace("survey")) {
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc = ~fpc)
regmodel <- svyglm(api00 ~ ell * meals + sch.wide, design = dstrat)
probe_interaction(model = regmodel, pred = ell, modx = meals,
                  modx.values = "plus-minus", cond.int = TRUE)

# 3-way with survey and factor input
regmodel3 <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
probe_interaction(model = regmodel3, pred = ell, modx = meals,
                  mod2 = sch.wide)
# Can try different configurations of 1st vs 2nd mod
probe_interaction(model = regmodel3, pred = ell, modx = sch.wide,
                  mod2 = meals)
}
#> Warning: Johnson-Neyman intervals are not available for factor predictors or
#> moderators.
#> ████████████████ While meals (2nd moderator) = 18.72139 (- 1 SD) ███████████████ 
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of ell when sch.wide = No: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -3.51   1.99    -1.76   0.08
#> 
#> Slope of ell when sch.wide = Yes: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.93   0.83    -1.12   0.26
#> 
#> █████████████████ While meals (2nd moderator) = 48.22427 (Mean) ████████████████ 
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of ell when sch.wide = No: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -1.98   1.25    -1.58   0.11
#> 
#> Slope of ell when sch.wide = Yes: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.62   0.56    -1.12   0.26
#> 
#> ████████████████ While meals (2nd moderator) = 77.72716 (+ 1 SD) ███████████████ 
#> 
#> SIMPLE SLOPES ANALYSIS
#> 
#> Slope of ell when sch.wide = No: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.45   1.23    -0.37   0.71
#> 
#> Slope of ell when sch.wide = Yes: 
#> 
#>    Est.   S.E.   t val.      p
#> ------- ------ -------- ------
#>   -0.32   0.38    -0.84   0.40
#> 

```

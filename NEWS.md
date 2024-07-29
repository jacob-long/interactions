# interactions 1.2.0

A special thanks to Github user @olivroy for contributing a number of mostly 
under-the-hood updates prior to this release.

Enhancements:

* `sim_slopes()` now supports non-continuous variables in the `pred` argument.
* `sim_slopes()` now has an `at` argument, allowing you to specify an exact,
perhaps non-centered, level for variables not involved in the interaction.
* `interact_plot()` now has provisional support for factor predictors (`pred`).
Users will receive a message because of the possibility for unexpected 
behavior. `cat_plot()` likewise has support for continuous moderators. (#54)
* Website and some documentation have been revamped and upgraded.
* Users can now change the axis labels for `johnson_neyman()` plots via the 
arguments `y.label` and `modx.label`. (#56)
* Models produced by the `panelr` package are better supported.

Bug fixes:

* `johnson_neyman()` now handles non-syntactic variable names for `modx` 
correctly. (#56)
* `sim_slopes()` no longer displays results with factor moderators in the 
reverse order of the factor's levels. (#55)
* `probe_interactions()` no longer errors when certain combinations of arguments
are provided. (#50)
* `sim_slopes()` no longer errors when ordered factors are moderators.
Thanks to Jonathan Zadra for suggesting the fix. (#42)
* `sim_slopes()` no longer errors when given `merModTest` objects.

# interactions 1.1.5

* Made a small change to avoid testing errors in a forthcoming R release.

# interactions 1.1.4

Bugfix:

* `sim_slopes()` now correctly handles the `robust` argument when it is not
set to `TRUE` or `FALSE`. Many thanks to Andy Field for reporting the issue. 
(#36)

# interactions 1.1.3

Minor fix:

* Plotting functions no longer fail when there is missing data in the moderator
variable(s).

# interactions 1.1.2

Bugfixes: 

* Plotting functions no longer fail with incomplete source data.
* Plotting functions now respect the order of `modx.values` and `mod2.values`
arguments. (#29)
* `interact_plot()` no longer ignores the `point.alpha` argument. (#25)
* Plotting functions now allow you to specify labels for `modx.values` or
`mod2.values` by passing a named vector to those arguments. (#30)
* `sim_slopes()` now prints labels when requested with the `modx.labels` or
`mod2.labels` arguments. (#32)

Feature update:

* Plotting functions now better support `brmsfit` objects, in particular 
those with multiple dependent variables and distributional dependent variables.
Use the `resp` and `dpar` arguments to specify which you want to use. 

# interactions 1.1.1

Bugfixes:

* `sim_slopes()` no longer fails getting Johnson-Neyman intervals for `merMod`
models. (#20)
* `cat_plot()` no longer ignores `pred.values` and `pred.labels` arguments.
Thanks to [Paul Djupe](http://pauldjupe.com/) for alerting me to this.
* The `tidy()` method for `sim_slopes` objects no longer returns numbers as
strings. This had downstream effects on, e.g., the `plot()` method for 
`sim_slopes`. (#22; thanks to Noah Greifer)
* `sim_slopes()` now handles `lmerModTest` objects properly. Thanks to Eric 
Shuman for bringing it to my attention.

# interactions 1.1.0

## New function: `sim_margins()`

This is, as the name suggests, related to `sim_slopes()`. However, instead of
*slopes*, what is being estimated are 
[marginal effects](https://cran.r-project.org/package=margins/vignettes/TechnicalDetails.pdf). 
In the case of OLS linear regression, this is basically the same thing. The 
slope in OLS is the expected change in the outcome for each 1-unit increase in
the predictor. For other models, however, the actual change in the outcome
when there's a 1-unit increase in a variable depends on the level of other 
covariates and the initial value of the predictor. In a logit model, 
for instance, the change in probability will be different if the initial 
probability was 50% (could go quite a bit up or down) than if it was 99.9% 
(can't go up).

`sim_margins()` uses the [`margins`](https://cran.r-project.org/package=margins)
package under the hood to estimate marginal effects. Unlike `sim_slopes()`, 
in which by default all covariates not involved in the interaction are 
mean-centered, in `sim_margins()` these covariates are always left at their
observed values because they influence the level of the marginal effect. 
Instead, the marginal effect is calculated with the covariates and focal 
predictor (`pred`) at their observed values and the moderator(s) held at the
specified values (e.g., the mean and 1 standard deviation above/below the mean).
I advise using `sim_margins()` rather than `sim_slopes()` when analyzing models
other than OLS regression.

## Bug fixes

* `interact_plot()` and `cat_plot()` now respect the user's selection of 
`outcome.scale`; in 1.0.0, it always plotted on the response scale. (#12)
* The `modx.values` argument is now better documented to explain that you may
use it to specify the exact values you want. Thanks to Jakub Lysek for asking
the question that prompted this. (#8)
* `modx.values` now accepts `"mean-plus-minus"` as a manual specification of
the default auto-calculated values for continuous moderators. `NULL` still 
defaults to this, but you can now make this explicit in your code if desired
for clarity or to guard against future changes in the default behavior.
* Users are now warned when `modx.values` or `mod2.values` include values
outside the observed range of the `modx`/`mod2`. (#9)
* Users are now warned when `pred`, `modx`, and `mod2` are not all involved in
an interaction with each other in the provided model. (#10)
* `cat_plot()` was ignoring `mod2.values` arguments but now works properly. 
(#17)
* Missing values in the original data are now handled better in 
`interact_plot()` and `cat_plot()`.
* `sim_slopes()` now handles non-syntactic variable names better.
* `interactions` now requires you to have a relatively new version of `rlang`.
Users with older versions were experiencing cryptic errors. (#15)

## Feature updates

* `interact_plot()` and `cat_plot()` now have an `at` argument for more granular
control over the values of covariates.
* `sim_slopes()` now allows for custom specification of robust standard error 
estimators via providing a function to `v.cov` and arguments to `v.cov.args`.

# interactions 1.0.0

This is the first release, but a look at the NEWS for
[`jtools`](https://jtools.jacob-long.com) prior to its version 2.0.0 will
give you an idea of the history of the functions in this package. 

What follows is an accounting of changes to functions in this package since
they were last in `jtools`.

* Plots made by `interactions` now have a new theme, which you can use yourself, 
called `theme_nice()` (from the `jtools` package). The previous default,
`theme_apa()`, is still available but I don't like it as a default since I don't
think the APA has defined the nicest-looking design guidelines for general use.
* `interact_plot()` now has appropriate coloring for observed data when the 
moderator is numeric (#1). In previous versions I had to use a workaround that 
involved tweaking the alpha of the observed data points. 
* `interact_plot()` and `cat_plot()` now use *tidy evaluation* for the `pred`,
`modx`, and `mod2` arguments. This means you can pass a variable that contains 
the name of `pred`/`modx`/`mod2`,
which is most useful if you are creating a function, for loop, etc. If using a
variable, put a `!!` from the `rlang` package before it
(e.g., `pred = !! variable`). For most users, these changes will not affect 
their usage.
* `sim_slopes()` no longer prints coefficient tables as data frames because this 
caused RStudio notebook users issues with the output not being printed to the
console and having the notebook format them in less-than-ideal ways. The tables
now have a markdown format that might remind you of Stata's coefficient tables.
Thanks to Kim Henry for contacting me about this.

## Use partial residuals for plotting 

One negative when visualizing predictions alongside original data 
with `interact_plot()` or similar
tools is that the observed data may be too spread out to pick up on any 
patterns. However, sometimes your model is controlling for the causes of this
scattering, especially with multilevel models that have random intercepts. 
Partial residuals include the effects of all the controlled-for variables 
and let you see how well your model performs with all of those things accounted
for. 

You can plot partial residuals instead of the observed data in `interact_plot()`
and `cat_plot()` via the argument `partial.residuals = TRUE`.

## Important changes to `make_predictions()` and removal of `plot_predictions()`

In the `jtools` 1.0.0 release, I introduced `make_predictions()` as a lower-level
way to emulate the functionality of `effect_plot()`, `interact_plot()`, and 
`cat_plot()`. This would return a list object with predicted data, the original 
data, and a bunch of attributes containing information about how to plot it.
One could then take this object, with class `predictions`, and use it as the 
main argument to `plot_predictions()`, which was another new function that 
creates the plots you would see in `effect_plot()` et al.

I have simplified `make_predictions()` to be less specific to those plotting 
functions and eliminated `plot_predictions()`, which was ultimately too complex
to maintain and caused problems for separating the interaction tools into a 
separate package. `make_predictions()` by default simply creates a new data frame
of predicted values along a `pred` variable. It no longer accepts `modx` or 
`mod2` arguments. Instead, it accepts an argument called `at` where a user can
specify any number of variables and values to generate predictions *at*. This
syntax is designed to be similar to the `predictions`/`margins` packages. See
the `jtools` documentation for more info on this revised syntax. 

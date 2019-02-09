# interactions 1.0.0

This is the first release, but a look at the NEWS for
[`jtools`](https://jtools.jacob-long.com) prior to its version 2.0.0 will
give you an idea of the history of the functions in this package. 

What follows is an accounting of changes to functions in this package since
they were last in `jtools`.

* Plots made by `interactions` now have a new theme, which you can use yourself, 
called `theme_nice` (from the `jtools` package). The previous default,
`theme_apa`, is still available but I don't like it as a default since I don't
think the APA has defined the nicest-looking design guidelines for general use.
* `interact_plot` now has appropriate coloring for observed data when the 
moderator is numeric (#1). In previous versions I had to use a workaround that 
involved tweaking the alpha of the observed data points. 
* `interact_plot` and `cat_plot` now use *tidy evaluation* for the `pred`,
`modx`, and `mod2` arguments. This means you can pass a variable that contains 
the name of `pred`/`modx`/`mod2`,
which is most useful if you are creating a function, for loop, etc. If using a
variable, put a `!!` from the `rlang` package before it
(e.g., `pred = !! variable`). For most users, these changes will not affect 
their usage.
* `sim_slopes` no longer prints coefficient tables as data frames because this 
caused RStudio notebook users issues with the output not being printed to the
console and having the notebook format them in less-than-ideal ways. The tables
now have a markdown format that might remind you of Stata's coefficient tables.
Thanks to Kim Henry for contacting me about this.

## Use partial residuals for plotting 

One negative when visualizing predictions alongside original data 
with `interact_plot` or similar
tools is that the observed data may be too spread out to pick up on any 
patterns. However, sometimes your model is controlling for the causes of this
scattering, especially with multilevel models that have random intercepts. 
Partial residuals include the effects of all the controlled-for variables 
and let you see how well your model performs with all of those things accounted
for. 

You can plot partial residuals instead of the observed data in `interact_plot`
and `cat_plot` via the argument `partial.residuals = TRUE`.

## Important changes to `make_predictions` and removal of `plot_predictions`

In the `jtools` 1.0.0 release, I introduced `make_predictions` as a lower-level
way to emulate the functionality of `effect_plot`, `interact_plot`, and 
`cat_plot`. This would return a list object with predicted data, the original 
data, and a bunch of attributes containing information about how to plot it.
One could then take this object, with class `predictions`, and use it as the 
main argument to `plot_predictions`, which was another new function that 
creates the plots you would see in `effect_plot` et al.

I have simplified `make_predictions` to be less specific to those plotting 
functions and eliminated `plot_predictions`, which was ultimately too complex
to maintain and caused problems for separating the interaction tools into a 
separate package. `make_predictions` by default simply creates a new data frame
of predicted values along a `pred` variable. It no longer accepts `modx` or 
`mod2` arguments. Instead, it accepts an argument called `at` where a user can
specify any number of variables and values to generate predictions *at*. This
syntax is designed to be similar to the `predictions`/`margins` packages. See
the `jtools` documentation for more info on this revised syntax. 

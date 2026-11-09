# Tidiers for [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md) objects.

You can use
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
and
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
for "tidy" methods of storing `sim_slopes` output.

## Usage

``` r
# S3 method for class 'sim_slopes'
tidy(x, conf.level = 0.95, ...)

# S3 method for class 'sim_slopes'
glance(x, ...)
```

## Arguments

- x:

  The `sim_slopes` object

- conf.level:

  The width of confidence intervals. Default is .95 (95%).

- ...:

  Ignored.

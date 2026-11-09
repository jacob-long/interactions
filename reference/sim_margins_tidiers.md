# Tidiers for [`sim_margins()`](https://interactions.jacob-long.com/reference/sim_margins.md) objects.

You can use
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
and
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
for "tidy" methods of storing `sim_margins` output.

## Usage

``` r
# S3 method for class 'sim_margins'
tidy(x, conf.level = 0.95, ...)

# S3 method for class 'sim_margins'
glance(x, ...)
```

## Arguments

- x:

  The `sim_margins` object

- conf.level:

  The width of confidence intervals. Default is .95 (95%).

- ...:

  Ignored.

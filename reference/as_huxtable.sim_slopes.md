# Create tabular output for simple slopes analysis

This function converts a `sim_slopes` object into a `huxtable` object,
making it suitable for use in external documents.

## Usage

``` r
as_huxtable.sim_slopes(
  x,
  format = "{estimate} ({std.error})",
  sig.levels = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, `#` = 0.1),
  digits = getOption("jtools-digits", 2),
  conf.level = 0.95,
  intercept = attr(x, "cond.int"),
  int.format = format,
  ...
)
```

## Arguments

- x:

  The
  [`sim_slopes()`](https://interactions.jacob-long.com/reference/sim_slopes.md)
  object.

- format:

  The method for sharing the slope and associated uncertainty. Default
  is `"{estimate} ({std.error})"`. See the instructions for the
  `error_format` argument of
  [`jtools::export_summs()`](https://jtools.jacob-long.com/reference/export_summs.html)
  for more on your options.

- sig.levels:

  A named vector in which the values are potential p value thresholds
  and the names are significance markers (e.g., "\*") for when p values
  are below the threshold. Default is
  `` c(`***` = .001, `**` = .01, `*` = .05, `#` = .1) ``.

- digits:

  How many digits should the outputted table round to? Default is 2.

- conf.level:

  How wide the confidence interval should be, if it is used. .95 (95%
  interval) is the default.

- intercept:

  Should conditional intercepts be included? Default is whatever the
  `cond.int` argument to `x` was.

- int.format:

  If conditional intercepts were requested, how should they be
  formatted? Default is the same as `format`.

- ...:

  Ignored.

## Details

For more on what you can do with a `huxtable`, see huxtable.

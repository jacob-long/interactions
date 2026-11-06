test_that("panelr slope presentation methods do not require refitted models", {
  skip_if_not_installed("panelr")
  skip_if(!"wb_slope_design" %in% getNamespaceExports("panelr"))
  d <- panelr::panel_data(panelr::WageData, id = id, wave = t)
  m <- suppressMessages(panelr::wbm(lwage ~ wks * exp, d, pR2 = FALSE, pvals = FALSE))
  ss <- sim_slopes(m, wks, exp, cond.int = TRUE, confint = TRUE)
  expect_equal(nobs(ss), nrow(m@frame))
  expect_length(ss$mods, 0)
  expect_output(print(ss), "SIMPLE SLOPES")
  expect_silent(ggplot2::ggplot_build(plot(ss)))
  caption <- plot(ss)$labels$caption
  expect_match(caption, "Fixed-effect slopes: response scale")
  expect_match(caption, "not Satterthwaite/Kenward-Roger", fixed = TRUE)
  expect_match(caption, "0 = the person's observed mean", fixed = TRUE)
  expect_match(caption, "Reference person means", fixed = TRUE)
  expect_match(caption, "interaction-centering terms held fixed", fixed = TRUE)
  expect_lt(nchar(caption), nchar(attr(ss, "panelr")$note))
  expect_identical(ss$jn[[1]]$plot$labels$caption, caption)
  legacy <- ss
  attr(legacy, "panelr")$caption <- NULL
  expect_identical(plot(legacy)$labels$caption, attr(ss, "panelr")$note)
  if (requireNamespace("huxtable", quietly = TRUE)) {
    expect_s3_class(huxtable::as_huxtable(ss), "huxtable")
    expect_s3_class(huxtable::as_huxtable(ss, intercept = FALSE), "huxtable")
  }
  var <- "wks"
  expect_s3_class(probe_interaction(m, !!rlang::sym(var), exp), "probe_interaction")
  expect_s3_class(johnson_neyman(m, "wks", "exp", plot = FALSE), "johnson_neyman_contrast")
  labeled <- johnson_neyman(m, wks, exp, plot = FALSE, modx.label = "Experience deviation")
  expect_identical(labeled$plot$labels$x, "Experience deviation")
})

test_that("three-way panelr Johnson-Neyman plots render with shared scale metadata", {
  skip_if_not_installed("panelr")
  skip_if_not_installed("cowplot")
  skip_if(!"wb_slope_design" %in% getNamespaceExports("panelr"))
  set.seed(813)
  d <- data.frame(id = rep(1:24, each = 6), wave = rep(1:6, 24),
                   x = rnorm(144), z = rnorm(144), w = rnorm(144))
  d$y <- d$x * (1 + d$z + d$w + d$z * d$w) + rnorm(144)
  m <- suppressMessages(panelr::wbm(y ~ x * z * w, panelr::panel_data(d),
                                     pR2 = FALSE, pvals = FALSE))
  ss <- sim_slopes(m, x, z, mod2 = w, mod2.values = c(-1, 1),
                    jnplot = TRUE, confint = TRUE)
  expect_length(ss$jn, 2)
  expect_s3_class(ss$jnplot, "ggplot")
  expect_silent(ggplot2::ggplot_build(ss$jnplot))
  expect_silent(ggplot2::ggplot_build(plot(ss)))
  expect_equal(nrow(generics::tidy(ss)), 6)
})

test_that("probe_interaction resolves ordinary models in local environments", {
  local_probe <- function() {
    local_model <- lm(mpg ~ hp * wt, mtcars)
    name <- "hp"
    probe_interaction(local_model, pred = !!rlang::sym(name), modx = wt,
                        johnson_neyman = FALSE)
  }
  expect_s3_class(local_probe(), "probe_interaction")
})

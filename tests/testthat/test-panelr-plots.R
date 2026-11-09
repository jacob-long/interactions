test_that("non-panel models bypass component-aware plot preparation", {
  m <- lm(mpg ~ hp * wt, mtcars)
  expect_null(prepare_panel_plot(m, mtcars, "hp", "wt", NULL,
                                "within", NULL, NULL, NULL))
  p <- interact_plot(m, hp, wt)
  expect_null(attr(p, "panelr"))
  expect_null(p$labels$caption)
  expect_equal(range(p$data$hp), range(mtcars$hp))
})

test_that("observed groups tolerate extrapolated and single moderator values", {
  d <- data.frame(x = -3:3, z = -3:3)
  values <- c(low = 10, middle = 20, high = 30)
  out <- split_int_data(d, "x", "z", FALSE, unname(values), values,
                        unname(values), values, FALSE, FALSE)
  expect_equal(as.character(out$modx_group), rep("low", 7))
  expect_equal(as.character(out$mod2_group), rep("low", 7))
  out <- split_int_data(d, "x", "z", FALSE, 0, c(mean = 0),
                        0, c(mean = 0), FALSE, FALSE)
  expect_equal(as.character(out$modx_group), rep("mean", 7))
  expect_equal(as.character(out$mod2_group), rep("mean", 7))
})

test_that("panelr's fitted components determine native plot defaults", {
  skip_if_not_installed("panelr")
  skip_if(!"prepare_wb_plot" %in% getNamespaceExports("panelr"),
          "panelr needs the component-aware plotting hook")
  d <- panelr::panel_data(panelr::WageData, id = id, wave = t)
  m <- suppressMessages(panelr::wbm(lwage ~ wks * exp, d,
                                    pR2 = FALSE, pvals = FALSE))
  p <- interact_plot(m, wks, exp, interval = TRUE)
  expect_equal(range(p$data$wks), range(m@frame$wks))
  expect_equal(sort(unique(p$data$exp)), c(-1, 0, 1) * sd(m@frame$exp))
  expect_match(p$labels$x, "within-person deviation")
  expect_identical(p$labels$y, "Predicted lwage")
  expect_match(p$labels$caption, "Reference person means")
  expect_identical(attr(p, "panelr")$scale, "within")
  expect_null(attr(m, "panelr_plot"))
  expect_silent(ggplot2::ggplot_build(p))
  expect_warning(interact_plot(m, wks, exp, modx.values = c(10, 20, 30)),
                  "extrapolate")
  p <- suppressWarnings(interact_plot(m, wks, exp, plot.scale = "original"))
  expect_equal(range(p$data$wks), range(d$wks))
  expect_identical(p$labels$x, "wks")
  expect_match(p$labels$caption, "Original levels")
})

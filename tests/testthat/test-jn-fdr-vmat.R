test_that("johnson_neyman control.fdr = TRUE uses supplied vmat", {
  fit <- lm(mpg ~ hp * wt, data = mtcars)
  v_scaled <- 4 * vcov(fit)

  jn_ols <- johnson_neyman(fit, pred = hp, modx = wt, control.fdr = TRUE,
                           plot = FALSE)
  jn_vmat <- johnson_neyman(fit, pred = hp, modx = wt, vmat = v_scaled,
                            control.fdr = TRUE, plot = FALSE)

  # Expected Esarey & Sumner (2017) FDR t-critical value using v_scaled
  coefs <- coef(fit)
  mod_range <- range(mtcars$wt)
  rseq <- seq(from = mod_range[1], to = mod_range[2],
              by = (mod_range[2] - mod_range[1]) / 1000)
  me <- coefs["hp"] + coefs["hp:wt"] * rseq
  se <- sqrt(v_scaled["hp", "hp"] +
               rseq^2 * v_scaled["hp:wt", "hp:wt"] +
               2 * rseq * v_scaled["hp", "hp:wt"])
  df <- df.residual(fit)
  ps <- 2 * pmin(pt(me / se, df = df), 1 - pt(me / se, df = df))
  multipliers <- seq_along(me) / length(me)
  ps_o <- order(ps)
  test <- 0
  i <- length(me) + 1
  while (test == 0 && i > 1) {
    i <- i - 1
    test <- min(ps[ps_o][1:i] <= multipliers[i] * 0.05)
  }
  expected_tcrit <- abs(qt(multipliers[i] * 0.025, df))

  expect_equal(jn_vmat$t_value, expected_tcrit, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(jn_vmat$t_value, jn_ols$t_value)))

  # Bounds should match calling johnson_neyman with the same vmat and critical.t
  jn_ref <- johnson_neyman(fit, pred = hp, modx = wt, vmat = v_scaled,
                           critical.t = expected_tcrit, plot = FALSE)
  expect_equal(jn_vmat$bounds, jn_ref$bounds, tolerance = 1e-10)
})

test_that("sim_slopes robust + control.fdr passes robust vcov into FDR step", {
  skip_if_not_installed("sandwich")
  set.seed(42)
  n <- 100
  x <- rnorm(n)
  m <- rnorm(n)
  y <- 1 + 0.8 * x + 0.5 * m + 0.6 * x * m + rnorm(n, sd = exp(1.2 * abs(m)))
  fit <- lm(y ~ x * m)
  v_hc3 <- sandwich::vcovHC(fit, type = "HC3")

  jn_ols <- johnson_neyman(fit, pred = x, modx = m, control.fdr = TRUE,
                           plot = FALSE)
  jn_hc3 <- johnson_neyman(fit, pred = x, modx = m, vmat = v_hc3,
                           control.fdr = TRUE, plot = FALSE)
  ss_hc3 <- sim_slopes(fit, pred = x, modx = m, robust = "HC3",
                       control.fdr = TRUE)

  expect_false(isTRUE(all.equal(jn_hc3$t_value, jn_ols$t_value)))
  expect_equal(ss_hc3$jn[[1]]$t_value, jn_hc3$t_value, tolerance = 1e-10)
  expect_equal(ss_hc3$jn[[1]]$bounds, jn_hc3$bounds, tolerance = 1e-10)
})

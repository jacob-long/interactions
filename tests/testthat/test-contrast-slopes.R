test_that("contrast slopes reproduce lm refits with fitting weights preserved", {
  set.seed(87)
  d <- data.frame(x = rnorm(120), z = rnorm(120), w = rnorm(120),
                   weight = runif(120, .5, 2))
  d$y <- 2 + .5 * d$x + d$z + 1.2 * d$x * d$z + .4 * d$x * d$z * d$w + rnorm(120)
  for (weighted in c(FALSE, TRUE)) {
    m <- if (weighted) lm(y ~ x * z * w, d, weights = weight) else lm(y ~ x * z * w, d)
    old <- sim_slopes(m, x, z, mod2 = w, modx.values = c(-1, 0, 1),
                       mod2.values = c(-.5, .5), johnson_neyman = FALSE, confint = TRUE)
    for (i in seq_along(old$slopes)) {
      table <- old$slopes[[i]]
      grid <- data.frame(x = 0, z = table[[1]], w = unname(attr(old, "mod2.values")[i]))
      X0 <- model.matrix(delete.response(terms(m)), grid)
      grid$x <- 1
      L <- model.matrix(delete.response(terms(m)), grid) - X0
      direct <- contrast_inference(L, coef(m), vcov(m), df.residual(m), .95)
      expect_equal(direct, table[, -1], tolerance = 1e-8)
      for (j in seq_len(nrow(grid))) {
        shifted <- d
        shifted$z <- d$z - grid$z[j]
        shifted$w <- d$w - grid$w[j]
        refit <- if (weighted) lm(y ~ x * z * w, shifted, weights = weight) else
          lm(y ~ x * z * w, shifted)
        coefficient <- summary(refit)$coefficients["x", ]
        expect_equal(direct$Est.[j], unname(coefficient[1]))
        expect_equal(direct$S.E.[j], unname(coefficient[2]))
        expect_equal(direct$p[j], unname(coefficient[4]))
      }
      expected <- coef(m)["x"] + coef(m)["x:z"] * grid$z +
        coef(m)["x:w"] * grid$w + coef(m)["x:z:w"] * grid$z * grid$w
      expect_equal(direct$Est., unname(expected))
    }
    expect_length(old$mods, 6)
  }
})

test_that("contrast covariance includes off-diagonal terms", {
  beta <- c(1, 2)
  V <- matrix(c(4, 1.5, 1.5, 9), 2)
  z <- c(-1, 0, 2)
  out <- contrast_inference(cbind(1, z), beta, V, 20, .9)
  expect_equal(out$Est., 1 + 2 * z)
  expect_equal(out$S.E., sqrt(4 + 3 * z + 9 * z^2))
  expect_equal(out$p, 2 * pt(-abs(out$Est. / out$S.E.), 20))
  expect_equal(out[["5%"]], out$Est. - qt(.95, 20) * out$S.E.)
})

test_that("Johnson-Neyman roots agree with the existing ordinary lm calculation", {
  set.seed(551)
  d <- data.frame(x = rnorm(150), z = rnorm(150))
  d$y <- 1 + .4 * d$x + d$z + 1.1 * d$x * d$z + rnorm(150)
  m <- lm(y ~ x * z, d)
  old <- johnson_neyman(m, x, z, plot = FALSE)
  coefs <- c("x", "x:z")
  direct <- contrast_johnson_neyman(coef(m)[coefs], vcov(m)[coefs, coefs],
                                    range(d$z), "x", "z", df.residual(m))
  expect_equal(direct$bounds, old$bounds, tolerance = 1e-10)
  for (z in unname(direct$bounds)) {
    s <- drop(c(1, z) %*% coef(m)[coefs])
    se <- sqrt(drop(c(1, z) %*% vcov(m)[coefs, coefs] %*% c(1, z)))
    expect_equal(abs(s) / se, qt(.975, df.residual(m)), tolerance = 1e-8)
  }
  expect_silent(ggplot2::ggplot_build(direct$plot))
})

test_that("Johnson-Neyman handles degenerate, tangent, and linear crossings", {
  V <- diag(c(1, 0))
  none <- contrast_johnson_neyman(c(0, 0), V, c(-2, 2), "x", "z")
  all <- contrast_johnson_neyman(c(10, 0), V, c(-2, 2), "x", "z")
  expect_length(none$bounds, 0)
  expect_false(any(none$intervals$significant))
  expect_length(all$bounds, 0)
  expect_true(all(all$intervals$significant))
  expect_output(print(none), "does not exclude zero")
  one <- contrast_johnson_neyman(c(1, 2), diag(2), c(-3, 3), "x", "z", critical.t = 2)
  expect_equal(unname(one$bounds), .75)
  expect_identical(one$intervals$significant, c(FALSE, TRUE))
  tangent <- contrast_johnson_neyman(c(0, 1), matrix(0, 2, 2), c(-2, 2), "x", "z")
  expect_equal(unname(tangent$bounds), 0)
  expect_true(all(tangent$intervals$significant))
  expect_equal(contrast_roots(1, 1e8, 1), c(-1e8, -1e-8), tolerance = 1e-8)
  expect_error(contrast_johnson_neyman(c(0, 1), diag(2), c(-2, 2), "x", "z", control.fdr = TRUE), "FDR")
})

test_that("normalized Johnson-Neyman coordinates preserve shifted units", {
  effect <- c(.4, 1.2)
  V <- matrix(c(.03, .005, .005, .02), 2)
  original <- contrast_johnson_neyman(effect, V, c(-2, 2), "x", "z")
  shifted <- contrast_johnson_neyman(effect, V, 1e8 + 1000 * c(-2, 2),
                                      "x", "z", center = 1e8, scale = 1000)
  expect_equal(unname((shifted$bounds - 1e8) / 1000), unname(original$bounds), tolerance = 1e-10)
  expect_equal(shifted$intervals$significant, original$intervals$significant)
  expect_equal(shifted$cbands[[2]], original$cbands[[2]], tolerance = 1e-10)
  expect_equal(shifted$cbands$Lower, original$cbands$Lower, tolerance = 1e-10)
})

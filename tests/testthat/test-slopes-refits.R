refit_fixture <- function() {
  set.seed(418)
  d <- data.frame(x = rnorm(100), z = rnorm(100), q = rnorm(100),
                   w = runif(100, .1, 3), exposure = runif(100, 1, 3))
  d$y <- .5 * d$x + .8 * d$x * d$z + .4 * d$q +
    log(d$exposure) + rnorm(100)
  d
}

expect_refit_slopes <- function(model, ...) {
  before <- model
  out <- sim_slopes(model, x, z, modx.values = c(-1, 0, 1),
                      johnson_neyman = FALSE, centered = "none", ...)
  z <- out$slopes[[1]]
  V <- vcov(model)
  expected <- coef(model)["x"] + coef(model)["x:z"] * z
  se <- sqrt(V["x", "x"] + 2 * z * V["x", "x:z"] + z^2 * V["x:z", "x:z"])
  expect_equal(out$slopes$Est., unname(expected), tolerance = 1e-8)
  expect_equal(out$slopes$S.E., unname(se), tolerance = 1e-8)
  for (refit in out$mods) {
    expect_equal(nobs(refit), nobs(model))
    expect_equal(stats::model.weights(model.frame(refit)), stats::model.weights(model.frame(model)))
    expect_equal(stats::model.offset(model.frame(refit)), stats::model.offset(model.frame(model)))
  }
  expect_identical(model, before)
  invisible(out)
}

test_that("weighted slopes retain the original fit and weight expressions", {
  d <- refit_fixture()
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w))
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w^2))
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = exp(z)))
  d[["sampling weight"]] <- d$w
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = `sampling weight`))
  d$w[1:5] <- 0
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w))
})

test_that("subsets and omitted observations are not applied twice", {
  d <- refit_fixture()
  d$y[30] <- NA
  d$w[40] <- NA
  d$z[50] <- NA
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w,
                         subset = 21:100, na.action = na.exclude))
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w,
                         subset = x > 0, na.action = na.omit))
  expect_refit_slopes(lm(y ~ x * z + q, d, subset = 21:100))
})

test_that("local vector weights and formula arguments remain usable", {
  fit_local <- function(form) {
    original <- refit_fixture()
    prior <- original$w
    environment(form) <- environment()
    lm(form, original, weights = prior, subset = 11:100)
  }
  expect_refit_slopes(fit_local(y ~ x * z + q))
})

test_that("argument and formula offsets are retained exactly once", {
  d <- refit_fixture()
  expect_refit_slopes(lm(y ~ x * z + q, d, weights = w, offset = log(exposure)))
  expect_refit_slopes(lm(y ~ x * z + q + offset(log(exposure)), d, weights = w))
  expect_refit_slopes(lm(y ~ x * z + q + offset(log(exposure)), d,
                         weights = w, offset = q / 2, subset = 11:100))
})

test_that("weighted generalized refits retain link-scale slopes", {
  d <- refit_fixture()
  set.seed(419)
  d$y <- rpois(nrow(d), exp(.2 + .2 * d$x + .2 * d$x * d$z + log(d$exposure)))
  expect_refit_slopes(glm(y ~ x * z, d, family = poisson, weights = w,
                          offset = log(exposure), subset = 11:100))
})

test_that("the update helper distinguishes omission from explicit removal", {
  d <- refit_fixture()
  m <- lm(y ~ x * z, d, weights = w, offset = log(exposure))
  kept <- j_update(m)
  expect_equal(coef(kept), coef(m))
  expect_equal(stats::model.weights(model.frame(kept)), d$w)
  expect_equal(stats::model.offset(model.frame(kept)), log(d$exposure))
  removed <- j_update(m, weights = NULL, offset = NULL)
  expect_null(stats::model.weights(model.frame(removed)))
  expect_null(stats::model.offset(model.frame(removed)))
})

test_that("weighted centering and conditional intercepts preserve the offset", {
  d <- refit_fixture()
  m <- lm(y ~ x * z + q, d, weights = w, offset = log(exposure))
  b <- coef(m)
  for (at in list(NULL, list(q = 2))) {
    out <- sim_slopes(m, x, z, modx.values = c(-1, 0, 1),
                       cond.int = TRUE, at = at, johnson_neyman = FALSE)
    q <- if (is.null(at)) weighted.mean(d$q, d$w) else at$q
    expected <- b["(Intercept)"] + b["z"] * out$ints[[1]] + b["q"] * q
    expect_equal(out$ints$Est., unname(expected))
    expect_equal(out$slopes$Est., unname(b["x"] + b["x:z"] * out$slopes[[1]]))
  }
  m <- lm(y ~ x * z + q, d)
  out <- sim_slopes(m, x, z, modx.values = 0, at = list(q = 2), johnson_neyman = FALSE)
  expect_equal(out$ints$Est., unname(coef(m)["(Intercept)"] + 2 * coef(m)["q"]))
})

test_that("robust slopes and Johnson-Neyman analysis use weighted fits", {
  skip_if_not_installed("sandwich")
  d <- refit_fixture()
  m <- lm(y ~ x * z + q, d, weights = w, subset = 11:100)
  for (robust in list(FALSE, "HC3")) {
    out <- sim_slopes(m, x, z, modx.values = c(-1, 0, 1), robust = robust,
                       centered = "none")
    V <- if (identical(robust, FALSE)) vcov(m) else sandwich::vcovHC(m, type = robust)
    z <- out$slopes[[1]]
    se <- sqrt(V["x", "x"] + 2 * z * V["x", "x:z"] + z^2 * V["x:z", "x:z"])
    expect_equal(out$slopes$S.E., unname(se), tolerance = 1e-8)
    expected_jn <- johnson_neyman(m, x, z, vmat = V, plot = FALSE)
    expect_equal(out$jn[[1]]$bounds, expected_jn$bounds, tolerance = 1e-8)
  }
})

test_that("explicit raw data are aligned and checked before transformed refits", {
  d <- refit_fixture()
  d$q <- exp(d$q)
  d$y[30] <- NA
  m <- lm(y ~ x * z + log(q), d, weights = w, subset = 21:100)
  expect_refit_slopes(m, data = d[100:1, ])
  expect_refit_slopes(m)
  changed <- d
  changed$q[40] <- changed$q[40] * 2
  expect_error(sim_slopes(m, x, z, data = changed), "do not reproduce")
  expect_error(sim_slopes(m, x, z, data = d[-40, ]), "every fitted observation")
  set.seed(420)
  d$group <- factor(sample(c("a", "b"), nrow(d), TRUE))
  m <- lm(y ~ x * z + group, d, weights = w, subset = 21:100)
  expect_refit_slopes(m, data = d)
})

test_that("three-way refits retain weights and offsets for every moderator", {
  d <- refit_fixture()
  m <- lm(y ~ x * z * q, d, weights = w, offset = log(exposure), subset = 11:100)
  out <- sim_slopes(m, x, z, mod2 = q, modx.values = c(-1, 0, 1),
                     mod2.values = c(-1, 1), johnson_neyman = FALSE)
  for (j in seq_along(out$slopes)) {
    z <- out$slopes[[j]][[1]]
    q <- unname(attr(out, "mod2.values")[j])
    L <- matrix(0, length(z), length(coef(m)), dimnames = list(NULL, names(coef(m))))
    L[, "x"] <- 1
    L[, "x:z"] <- z
    L[, "x:q"] <- q
    L[, "x:z:q"] <- z * q
    expect_equal(out$slopes[[j]]$Est., unname(drop(L %*% coef(m))))
    expect_equal(out$slopes[[j]]$S.E., unname(sqrt(rowSums((L %*% vcov(m)) * L))))
  }
  for (refit in out$mods) {
    expect_equal(stats::model.weights(model.frame(refit)), stats::model.weights(model.frame(m)))
    expect_equal(stats::model.offset(model.frame(refit)), stats::model.offset(model.frame(m)))
  }
})

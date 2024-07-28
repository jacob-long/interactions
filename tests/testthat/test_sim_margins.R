skip_if_not_installed("margins")
context("sim_margins lm")

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
states$o70 <- 0
states$o70[states$`Life Exp` > 70] <- 1
states$o70n <- states$o70
states$o70 <- factor(states$o70)
states$o70l <- states$`Life Exp` > 70
states$o70c <- ifelse(states$o70l, yes = "yes", no = "no")
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states)
fit2 <- lm(Income ~ HSGrad*o70, data = states)
fit2n <- lm(Income ~ HSGrad*o70n, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states,
           weights = wts)
fitl <- lm(Income ~ HSGrad*o70l, data = states)
fitc <- lm(Income ~ HSGrad*Murder + o70c, data = states)

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
}

test_that("sim_margins works for lm", {
  expect_s3_class(sim_margins(model = fit,
                           pred = Murder,
                           modx = Illiteracy,
                           mod2 = HSGrad), "sim_margins")
  expect_s3_class(sim_margins(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              vce = "bootstrap",
                              iterations = 50), "sim_margins")
  expect_s3_class(sim_margins(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              vce = "simulation",
                              iterations = 50), "sim_margins")
})

test_that("sim_margins works for weighted lm", {
  expect_s3_class(sim_margins(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             modx.values = c(1.0, 1.5, 2.0)), class = "sim_margins")
  expect_s3_class(sim_margins(model = fitw,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              vce = "bootstrap",
                              modx.values = c(1.0, 1.5, 2.0),
                              iterations = 50), class = "sim_margins")
  expect_s3_class(sim_margins(model = fitw,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              vce = "simulation",
                              modx.values = c(1.0, 1.5, 2.0),
                              iterations = 50), class = "sim_margins")
})

test_that("sim_margins works for lm w/ logical", {
  expect_s3_class(sim_margins(model = fitl,
                              pred = HSGrad,
                              modx = o70l), "sim_margins")
  expect_s3_class(sim_margins(model = fitl,
                              pred = HSGrad,
                              modx = o70l,
                              vce = "bootstrap",
                              iterations = 50), "sim_margins")
  expect_s3_class(sim_margins(model = fitl,
                              pred = HSGrad,
                              modx = o70l,
                              vce = "simulation",
                              iterations = 50), "sim_margins")
})

test_that("sim_margins works for lm w/ non-focal character", {
  expect_s3_class(sim_margins(model = fitc,
                              pred = HSGrad,
                              modx = Murder), "sim_margins")
  expect_s3_class(sim_margins(model = fitc,
                              pred = HSGrad,
                              modx = Murder,
                              vce = "bootstrap",
                              iterations = 50), "sim_margins")
  expect_s3_class(sim_margins(model = fitc,
                              pred = HSGrad,
                              modx = Murder,
                              vce = "simulation",
                              iterations = 50), "sim_margins")
})

context("sim_margins methods")

test_that("as_huxtable.sim_margins works", {
  skip_if_not_installed("huxtable")
  skip_if_not_installed("broom")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    mod2 = HSGrad)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy)
  expect_is(as_huxtable.sim_margins(ss3), "huxtable")
  expect_is(as_huxtable.sim_margins(ss), "huxtable")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                     mod2 = HSGrad, vce = "bootstrap", iterations = 50)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    vce = "bootstrap", iterations = 50)
  expect_is(as_huxtable.sim_margins(ss3), "huxtable")
  expect_is(as_huxtable.sim_margins(ss), "huxtable")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                     mod2 = HSGrad, vce = "simulation", iterations = 50)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    vce = "simulation", iterations = 50)
  expect_is(as_huxtable.sim_margins(ss3), "huxtable")
  expect_is(as_huxtable.sim_margins(ss), "huxtable")
})

test_that("plot.sim_margins works", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("broom")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    mod2 = HSGrad)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy)
  expect_is(plot(ss3), "ggplot")
  expect_is(plot(ss), "ggplot")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                     mod2 = HSGrad, vce = "bootstrap", iterations = 50)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    vce = "bootstrap", iterations = 50)
  expect_is(plot(ss3), "ggplot")
  expect_is(plot(ss), "ggplot")
  ss3 <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                     mod2 = HSGrad, vce = "simulation", iterations = 50)
  ss <- sim_margins(model = fit, pred = Murder, modx = Illiteracy,
                    vce = "simulation", iterations = 50)
  expect_is(plot(ss3), "ggplot")
  expect_is(plot(ss), "ggplot")
})

context("sim_margins svyglm")

test_that("sim_margins works for svyglm", {
  skip_if_not_installed("survey")
  expect_is(sim_margins(regmodel, pred = ell, modx = meals, mod2 = both),
            "sim_margins")
  # margins bug
  # expect_is(sim_margins(regmodel, pred = ell, modx = meals, mod2 = both,
  #                       vce = "bootstrap", iterations = 50),
  #           "sim_margins")
  # expect_is(sim_margins(regmodel, pred = ell, modx = meals, mod2 = both,
  #                       vce = "simulation", iterations = 50),
  #           "sim_margins")
})

context("sim_margins merMod")

test_that("sim_margins works for lme4", {
  skip_if_not_installed("lme4")
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  fmVA0 <- glmer(r2 ~ Anger * Gender + btype + situ + (1|id) + (1|item),
                 family = binomial, data = VerbAgg, nAGQ=0L)
  lmVA0 <- lmer(as.numeric(r2 == "Y") ~ Anger * Gender + btype + situ +
                  (1|id) + (1|item), data = VerbAgg)
  
  expect_is(sim_margins(lmVA0, pred = Anger, modx = Gender), "sim_margins")
  expect_is(sim_margins(fmVA0, pred = Anger, modx = Gender), "sim_margins")
})

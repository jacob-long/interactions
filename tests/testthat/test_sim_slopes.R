context("sim_slopes lm")

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

library(ggplot2)
diamond <- diamonds
diamond <- diamond[diamond$color != "D",]
set.seed(10)
samps <- sample(1:nrow(diamond), 2000)
diamond <- diamond[samps,]
fitd <- lm(price ~ cut * color * clarity, data = diamond)

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
}

test_that("sim_slopes works for lm", {
  expect_silent(sim_slopes(model = fit,
                           pred = Murder,
                           modx = Illiteracy,
                           mod2 = HSGrad,
                           centered = "all"))
  expect_warning(sim_slopes(model = fit,
                            pred = Murder,
                            modx = Illiteracy,
                            mod2 = HSGrad,
                            centered = "HSGrad"))
})

test_that("sim_slopes works for weighted lm", {
  # Out of range warning
  expect_warning(sim_slopes(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             centered = "all"))
  expect_s3_class(suppressWarnings(sim_slopes(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             centered = "all")), class = "sim_slopes")
})

test_that("sim_slopes works for lm w/ logical", {
  expect_silent(sim_slopes(model = fitl,
                           pred = HSGrad,
                           modx = o70l,
                           johnson_neyman = FALSE))
})

test_that("sim_slopes works for lm w/ non-focal character", {
  expect_silent(sim_slopes(model = fitc,
                           pred = HSGrad,
                           modx = Murder,
                           johnson_neyman = FALSE))
})

test_that("sim_slopes accepts categorical predictor", {
  expect_warning(ss <- sim_slopes(fitd, pred = cut, modx = color))
  expect_s3_class(ss, "sim_slopes")
  expect_warning(ss <- sim_slopes(fitd, pred = cut, modx = color, mod2 = clarity))
  expect_s3_class(ss, "sim_slopes")
})

context("sim_slopes methods")

test_that("as_huxtable.sim_slopes works", {
  skip_if_not_installed("huxtable")
  skip_if_not_installed("broom")
  ss3 <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy,
                    mod2 = HSGrad)
  ss <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy)
  expect_is(as_huxtable.sim_slopes(ss3), "huxtable")
  expect_is(as_huxtable.sim_slopes(ss), "huxtable")
})
test_that("plot.sim_slopes works", {
  skip_if_not_installed("broom.mixed")
  skip_if_not_installed("broom")
  ss3 <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy,
                    mod2 = HSGrad)
  ss <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy)
  expect_is(plot(ss3), "ggplot")
  expect_is(plot(ss), "ggplot")
})

context("sim_slopes svyglm")

test_that("sim_slopes works for svyglm", {
  skip_if_not_installed("survey")
  expect_is(sim_slopes(regmodel, pred = ell, modx = meals, mod2 = both,
                       centered = "all"), "sim_slopes")
})

context("sim_slopes merMod")

test_that("sim_slopes works for lme4", {
  skip_if_not_installed("lme4")
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  fmVA0 <- glmer(r2 ~ Anger * Gender + btype + situ + (1|id) + (1|item),
                 family = binomial, data = VerbAgg, nAGQ=0L)
  lmVA0 <- lmer(as.numeric(r2 == "Y") ~ Anger * Gender + btype + situ +
                  (1|id) + (1|item), data = VerbAgg)
  
  expect_is(sim_slopes(lmVA0, pred = Anger, modx = Gender,
                       johnson_neyman = FALSE, t.df = "residual"),
            "sim_slopes")
  expect_is(sim_slopes(fmVA0, pred = Anger, modx = Gender,
                       johnson_neyman = FALSE), "sim_slopes")
})


### johnson_neyman ###########################################################

context("j_n specific")

test_that("johnson_neyman control.fdr argument works", {
  expect_s3_class(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                                 control.fdr = TRUE), "johnson_neyman")
})

test_that("johnson_neyman critical.t argument works", {
  expect_s3_class(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                                 critical.t = 2.1), "johnson_neyman")
})

test_that("johnson_neyman color arguments work", {
  expect_silent(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                               sig.color = "black", insig.color = "grey")$plot)
})

test_that("johnson_neyman mod.range argument works", {
  expect_silent(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                               mod.range = c(1, 2))$plot)
})

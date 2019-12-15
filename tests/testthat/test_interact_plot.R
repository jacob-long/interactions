context("interact_plot lm")

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

library(vdiffr)
Sys.setenv("VDIFFR_RUN_TESTS" = FALSE)

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
}

test_that("interact_plot works for lm", {
  plma <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                        mod2 = HSGrad, centered = "all")
  expect_doppelganger("plma", plma)
  expect_warning(
    plmm <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                          mod2 = HSGrad, centered = "HSGrad")
  )
  expect_doppelganger("plmm", plmm)
  plm1 <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                        mod2 = HSGrad, centered = "Area")
  expect_doppelganger("plm1", plm1)
  plm0 <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                        mod2 = HSGrad, centered = "none")
  expect_doppelganger("plm0", plm0)
})

test_that("interact_plot: robust standard errors work", {
  plmrob <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                        mod2 = HSGrad, centered = "all", robust = TRUE)
  expect_doppelganger("plmrob", plmrob)
})

test_that("rug plots work", {
  plmrugb <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                     mod2 = HSGrad, centered = "all", rug = TRUE)
  expect_doppelganger("plmrugb", plmrugb)

  plmruglb <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                            mod2 = HSGrad, centered = "all", rug = TRUE,
                            rug.sides = "lb")
  expect_doppelganger("plmruglb", plmruglb)
})


test_that("interact_plot works for weighted lm", {
  plmw <- interact_plot(model = fitw, pred = Murder, modx = Illiteracy,
                        mod2 = HSGrad, modx.values = c(1, 1.5, 2),
                        centered = "all")
  expect_doppelganger("plmw", plmw)
})

test_that("interact_plot works for lm w/ logical", {
  plmtf <- interact_plot(model = fitl, pred = HSGrad, modx = o70l)
  expect_doppelganger("plmtf", plmtf)
})

test_that("interact_plot works for lm w/ non-focal character", {
  plmnfchar <- interact_plot(model = fitc, pred = HSGrad, modx = Murder)
  expect_doppelganger("plmnfchar", plmnfchar)
})

test_that("interact_plot accepts user-specified values and labels", {
  plmlabelsc <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                              mod2 = HSGrad, centered = "all",
                              modxvals = c(1.5, 2, 2.5),
                              modx.labels = c("None","Low","High"),
                              mod2vals = c(58, 60, 62),
                              mod2.labels = c("Low","Average","High"))
  expect_doppelganger("plmlabelsc", plmlabelsc)

  # Alternate input
  plmlabelsc2 <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                              mod2 = HSGrad, centered = "all",
                              modxvals = c("None" = 1.5, "Low" = 2,
                                           "High" = 2.5),
                              mod2vals = c("Low" = 58, "Average" = 60,
                                           "High" = 62))
  expect_doppelganger("plmlabelsc2", plmlabelsc2)

  # Reject logical/factor pred
  expect_error(interact_plot(model = fit2, pred = o70, modx = HSGrad,
                             pred.labels = c("Under","Over")))
  plmlabelscpred <- interact_plot(model = fit2n, pred = o70n, modx = HSGrad,
                                  pred.labels = c("Under","Over"))
  expect_doppelganger("plmlabelscpred", plmlabelscpred)

  # Sort properly
  plmlabelscs <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                              mod2 = HSGrad, centered = "all",
                              modxvals = c(2.5, 2, 1.5),
                              modx.labels = c("High","Low","None"),
                              mod2vals = c(62, 60, 58),
                              mod2.labels = c("High","Average","Low"))
  expect_doppelganger("plmlabelscs", plmlabelscs)
})

test_that("interact_plot terciles modxval/mod2val works", {
  expect_message(
    plmtercs <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                              mod2 = HSGrad, modxvals = "terciles",
                              mod2vals = "terciles", centered = "none")
  )
  expect_doppelganger("plmtercs", plmtercs)
})

test_that("interact_plot linearity.check works", {
  plmlinearchp <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                                modxvals = "terciles", linearity.check = TRUE,
                                plot.points = TRUE, jitter = 0)
  expect_doppelganger("plmlinearchp", plmlinearchp)
  plmlinearchnp <- interact_plot(model = fit, pred = Murder, modx = Illiteracy,
                                 modxvals = "terciles", linearity.check = TRUE)
  expect_doppelganger("plmlinearchnp", plmlinearchnp)
})

context("interact_plot svyglm")

if (requireNamespace("survey")) {
  test_that("interact_plot works for svyglm", {
    psvya <- interact_plot(regmodel, pred = ell, modx = meals,  mod2 = both,
                           centered = "all")
    expect_doppelganger("psvya", psvya)
    expect_warning(
      psvy1 <- interact_plot(regmodel, pred = ell, modx = meals, mod2 = both,
                             centered = "ell")
    )
    expect_doppelganger("psvy1", psvy1)
  })
}

context("interact_plot merMod")

if (requireNamespace("lme4")) {
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  VerbAgg$mode_numeric <- as.numeric(VerbAgg$mode)
  mve <- lmer(Anger ~ Gender * mode + btype +  (1 | item), data = VerbAgg)
  mv <- lmer(Anger ~ Gender * mode_numeric + btype +  (1 | item),
             data = VerbAgg)
  gm <- glmer(incidence ~ period + (1 | herd), family = poisson, data = cbpp,
              offset = log(size))

  test_that("interact_plot works for lme4", {
    expect_error(interact_plot(mve, pred = mode, modx = Gender))
    plme4 <- interact_plot(mv, pred = mode_numeric, modx = Gender)
    expect_doppelganger("plme4", plme4)
    # expect_message(
      plme4i <- interact_plot(mv, pred = mode_numeric, modx = Gender,
                              interval = TRUE)
    # )
    expect_doppelganger("plme4i", plme4i)
  })

}

context("interact_plot offsets")

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

test_that("interact_plot handles offsets", {
  expect_message(
    pglmoff <- interact_plot(pmod, pred = talent, modx = money)
  )
  expect_doppelganger("pglmoff", pglmoff)
})

test_that("interact_plot handles offsets with robust SE", {
  expect_message(
    pglmrob <- interact_plot(pmod, pred = talent, modx = money, robust = TRUE)
  )
  expect_doppelganger("pglmrob", pglmrob)
})

test_that("sim_slopes handles offsets", {
  expect_s3_class(sim_slopes(pmod, pred = talent, modx = money), "sim_slopes")
})

### Code used to create brmsfit and stanreg test objects
# library(brms)
# fit1 <- brm(count ~ log_Age_c + log_Base4_c * Trt
#                    + (1 | patient) + (1 | obs),
#                  data = epilepsy, family = poisson(),
#                  prior = c(prior(student_t(5,0,10), class = b),
#                            prior(cauchy(0,2), class = sd)),
#                  cores = 2, chains = 2, iter = 2000)
# saveRDS(fit1, "brmfit.rds")
#
# library(rstanarm)
# fitrs <- stan_glmer(incidence ~ size * as.numeric(period) + (1 | herd),
#                   data = lme4::cbpp, family = poisson,
#                   # this next line is only to keep the example small in size!
#                   chains = 2, cores = 2, seed = 12345, iter = 500)
# saveRDS(fitrs, "rsafit.rds")

#### brms and rstanarm tests #################################################

if (requireNamespace("brms")) {
  context("brmsfit plots 2")
  bfit <- readRDS("brmfit.rds")
  test_that("brmsfit objects are supported", {
    pbfcat <- cat_plot(bfit, pred = "Trt", interval = TRUE)
    expect_doppelganger("pbfcat", pbfcat)
    pbfcont <- interact_plot(bfit, pred = "log_Base4_c", modx = "Trt",
                             interval = TRUE)
    expect_doppelganger("pbfcont", pbfcont)
  })
}

if (requireNamespace("rstanarm") & requireNamespace("lme4")) {
  context("stanreg plots")
  rsfit <- readRDS("rsafit.rds")
  library(lme4)
  data(cbpp)
  test_that("stanreg objects are supported", {
    prsacont <- interact_plot(rsfit, pred = "size", modx = "period",
                              interval = TRUE, data = cbpp)
    expect_doppelganger("prsacont", prsacont)
  })
}


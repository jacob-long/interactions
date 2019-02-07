context("interact_plot lm")

device <- getOption("device")
options(device = "pdf")

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

test_that("interact_plot works for lm", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all"))
  expect_silent(print(p))
  expect_warning(p <- interact_plot(model = fit,
                                    pred = Murder,
                                    modx = Illiteracy,
                                    mod2 = HSGrad,
                                    centered = "HSGrad"))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "Area"))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "none"))
  expect_silent(print(p))
})

test_that("interact_plot: robust standard errors work", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   robust = TRUE))
  expect_silent(print(p))
})

test_that("rug plots work", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   rug = TRUE))
  expect_silent(print(p))

  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   rug = TRUE,
                                   rug.sides = "lb"))
  expect_silent(print(p))
})


test_that("interact_plot works for weighted lm", {
  expect_silent(p <- interact_plot(model = fitw,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all"))
  expect_silent(print(p))
})

test_that("interact_plot works for lm w/ logical", {
  expect_silent(p <- interact_plot(model = fitl,
                                   pred = HSGrad,
                                   modx = o70l))
  expect_silent(print(p))
})

test_that("interact_plot works for lm w/ non-focal character", {
  expect_silent(sim_slopes(model = fitc,
                           pred = HSGrad,
                           modx = Murder,
                           johnson_neyman = FALSE))
  expect_silent(p <- interact_plot(model = fitc,
                                   pred = HSGrad,
                                   modx = Murder))
  expect_silent(print(p))
})

test_that("interact_plot accepts user-specified values and labels", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   modxvals = c(0, 1, 3),
                                   modx.labels = c("None","Low","High"),
                                   mod2vals = c(40, 60, 80),
                                   mod2.labels = c("Low","Average","High")))
  expect_silent(print(p))
  expect_error(p <- interact_plot(model = fit2,
                                  pred = o70,
                                  modx = HSGrad,
                                  pred.labels = c("Under","Over")))
  expect_silent(p <- interact_plot(model = fit2n,
                                   pred = o70n,
                                   modx = HSGrad,
                                   pred.labels = c("Under","Over")))
  expect_silent(print(p))
})

test_that("interact_plot terciles modxval/mod2val works", {
  expect_message(p <- interact_plot(model = fit,
                                    pred = Murder,
                                    modx = Illiteracy,
                                    mod2 = HSGrad,
                                    modxvals = "terciles",
                                    mod2vals = "terciles",
                                    centered = "none"))
  expect_silent(print(p))
})

test_that("interact_plot linearity.check works", {
  expect_message(p <- interact_plot(model = fit,
                                    pred = Murder,
                                    modx = Illiteracy,
                                    modxvals = "terciles",
                                    linearity.check = TRUE,
                                    plot.points = TRUE))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   linearity.check = TRUE))
  expect_silent(print(p))
})

context("interact_plot svyglm")

if (requireNamespace("survey")) {
  test_that("interact_plot works for svyglm", {
    expect_silent(p <- interact_plot(regmodel, pred = ell, modx = meals,
                                     mod2 = both,
                                     centered = "all"))
    expect_silent(print(p))
    expect_warning(p <- interact_plot(regmodel, pred = ell, modx = meals,
                                      mod2 = both,
                                      centered = "ell"))
    expect_silent(print(p))
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
    expect_error(p <- interact_plot(mve, pred = mode, modx = Gender))
    expect_silent(p <- interact_plot(mv, pred = mode_numeric, modx = Gender))
    expect_silent(print(p))
    expect_message(p <- interact_plot(mv, pred = mode_numeric, modx = Gender,
                                      interval = TRUE))
    expect_silent(print(p))
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
  expect_message(p <- interact_plot(pmod, pred = talent, modx = money))
  expect_silent(print(p))
})

test_that("interact_plot handles offsets with robust SE", {
  expect_message(p <- interact_plot(pmod, pred = talent, modx = money,
                                    robust = TRUE))
  expect_silent(print(p))
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
    expect_silent(print(cat_plot(bfit, pred = "Trt",
                                 interval = TRUE)))
    expect_silent(print(interact_plot(bfit, pred = "log_Base4_c", modx = "Trt",
                                      interval = TRUE)))
  })
}

if (requireNamespace("rstanarm") & requireNamespace("lme4")) {
  context("stanreg plots")
  rsfit <- readRDS("rsafit.rds")
  library(lme4)
  data(cbpp)
  test_that("stanreg objects are supported", {
    expect_silent(print(interact_plot(rsfit, pred = "size",
                                      modx = "period", interval = TRUE,
                                      data = cbpp)))
  })
}

options(device = device)
dev.off()

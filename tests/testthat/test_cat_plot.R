context("cat_plot lm")

library(vdiffr)
Sys.setenv("VDIFFR_RUN_TESTS" = FALSE)

if (requireNamespace("lme4", quietly = TRUE)) {
  library(lme4, quietly = TRUE)
  mv <- lmer(Anger ~ Gender*mode + btype +  (1 | item), data = VerbAgg)
}
library(ggplot2)
diamond <- diamonds
diamond <- diamond[diamond$color != "D",]
set.seed(10)
samps <- sample(1:nrow(diamond), 2000)
diamond <- diamond[samps,]
fit <- lm(price ~ cut * color, data = diamond)

test_that("cat_plot handles simple plot (bar)", {
  plmbar <- cat_plot(fit, pred = color, modx = cut)
  expect_doppelganger("plmbar", plmbar)
})

test_that("interact_plot handles simple cat plot (bar)", {
  plmbar <- interact_plot(fit, pred = color, modx = cut)
  expect_doppelganger("plmbar interact_plot", plmbar)
})

test_that("cat_plot handles intervals (bar)", {
  plmbari <- cat_plot(fit, pred = color, modx = cut, interval = TRUE)
  expect_doppelganger("plmbari", plmbari)
})

test_that("cat_plot handles plotted points (bar)", {
  plmbarpp <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                       plot.points = TRUE, jitter = 0)
  expect_doppelganger("plmbarpp", plmbarpp)
})

test_that("cat_plot handles simple plot (line)", {
  plmline <- cat_plot(fit, pred = color, modx = cut, geom = "line")
  expect_doppelganger("plmline", plmline)
})

test_that("cat_plot handles intervals (line)", {
  plmlinei <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                        geom = "line")
  expect_doppelganger("plmlinei", plmlinei)
})

test_that("interact_plot handles cat_plot intervals (line)", {
  plmlinei <- interact_plot(fit, pred = color, modx = cut, interval = TRUE,
                        geom = "line", data = diamond)
  expect_doppelganger("plmlinei interact_plot", plmlinei)
})

test_that("cat_plot handles plotted points (line)", {
  plmlinepp <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                        plot.points = TRUE, geom = "line", jitter = 0)
  expect_doppelganger("plmlinepp", plmlinepp)
})

test_that("cat_plot handles point.shape (line)", {
  plmlineps <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                        plot.points = TRUE, geom = "line", point.shape = TRUE,
                        jitter = 0)
  expect_doppelganger("plmlineps", plmlineps)
})

test_that("cat_plot handles linetypes (line)", {
  plmlinelt <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                        geom = "line", vary.lty = TRUE, plot.points = TRUE,
                        jitter = 0)
  expect_doppelganger("plmlinelt", plmlinelt)
})

test_that("cat_plot handles simple plot (point)", {
  plmpt <- cat_plot(fit, pred = color, modx = cut, geom = "point")
  expect_doppelganger("plmpt", plmpt)
})

test_that("cat_plot handles intervals (point)", {
  plmpti <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                     geom = "point")
  expect_doppelganger("plmpti", plmpti)
})

test_that("cat_plot handles plotted points (point)", {
  plmptpp <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                      plot.points = TRUE, geom = "point", jitter = 0)
  expect_doppelganger("plmptpp", plmptpp)
})

test_that("cat_plot handles point.shape (point)", {
  plmptps <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                      plot.points = TRUE, geom = "point", point.shape = TRUE,
                      jitter = 0)
  expect_doppelganger("plmptps", plmptps)
})

test_that("interact_plot handles cat_plot point.shape (point)", {
  plmptps <- interact_plot(fit, pred = color, modx = cut, interval = TRUE,
                      plot.points = TRUE, geom = "point", point.shape = TRUE,
                      jitter = 0, data = diamond)
  expect_doppelganger("plmptps interact_plot", plmptps)
})

test_that("cat_plot handles simple plot (boxplot)", {
  expect_error(cat_plot(fit, pred = color, modx = cut, geom = "boxplot"))
})

context("cat_plot glm")

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- rbinom(50, 1, .5)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
poisdat$talent <- factor(poisdat$talent)
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

test_that("cat_plot handles offsets", {
  pglmcatoff <- cat_plot(pmod, pred = talent)
  expect_doppelganger("pglmcatoff", pglmcatoff)
})

context("cat_plot survey")

test_that("cat_plot handles svyglm", {
  skip_if_not_installed("survey")
  library(survey, quietly = TRUE)
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
  
  psvycat <- cat_plot(regmodel, pred = both)
  expect_doppelganger("psvycat", psvycat)
})

context("cat_plot merMod")

test_that("cat_plot handles merMod", {
  skip_if_not_installed("lme4")
  plme4cat <- cat_plot(mv, pred = mode, modx = Gender, interval = FALSE)
  expect_doppelganger("plme4cat", plme4cat)
})

context("cat_plot 3-way")

## Will first create a couple dichotomous factors to ensure full rank
mpg2 <- mpg
mpg2$auto <- "auto"
mpg2$auto[mpg2$trans %in% c("manual(m5)", "manual(m6)")] <- "manual"
mpg2$auto <- factor(mpg2$auto)
mpg2$fwd <- "2wd"
mpg2$fwd[mpg2$drv == "4"] <- "4wd"
mpg2$fwd <- factor(mpg2$fwd)
## Drop the two cars with 5 cylinders (rest are 4, 6, or 8)
mpg2 <- mpg2[mpg2$cyl != "5",]
mpg2$cyl <- factor(mpg2$cyl)
## Fit the model
fit3 <- lm(cty ~ cyl * fwd * auto, data = mpg2)


test_that("cat_plot does 3-way interactions (bar)", {
  p3bar <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar")
  expect_doppelganger("p3bar", p3bar)
})

test_that("cat_plot does intervals w/ 3-way interactions (bar)", {
  p3bari <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                     interval = TRUE)
  expect_doppelganger("p3bari", p3bari)
})

test_that("cat_plot does plot.points w/ 3-way interactions (bar)", {
  p3barpp <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                      interval = TRUE, plot.points = TRUE, jitter = 0)
  expect_doppelganger("p3barpp", p3barpp)
})

test_that("cat_plot does point.shape w/ 3-way interactions (bar)", {
  p3barps <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                      interval = TRUE, plot.points = TRUE, point.shape = TRUE,
                      jitter = 0)
  expect_doppelganger("p3barps", p3barps)
})

test_that("cat_plot does 3-way interactions (line)", {
  p3line <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line")
  expect_doppelganger("p3line", p3line)
})

test_that("cat_plot does intervals w/ 3-way interactions (line)", {
  p3linei <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                      interval = TRUE)
  expect_doppelganger("p3linei", p3linei)
})

test_that("cat_plot does plot.points w/ 3-way interactions (line)", {
  p3linepp <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                       interval = TRUE, plot.points = TRUE, jitter = 0)
  expect_doppelganger("p3linepp", p3linepp)
})

test_that("cat_plot does point.shape w/ 3-way interactions (line)", {
  p3lineps <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                       interval = TRUE, plot.points = TRUE, point.shape = TRUE,
                       jitter = 0)
  expect_doppelganger("p3lineps", p3lineps)
})

test_that("cat_plot does vary.lty w/ 3-way interactions (line)", {
  p3linelty <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto,
                        geom = "line", interval = TRUE, plot.points = TRUE,
                        point.shape = TRUE, vary.lty = TRUE, jitter = 0)
  expect_doppelganger("p3linelty", p3linelty)
})

test_that("cat_plot does 3-way interactions (point)", {
  p3pt <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point")
  expect_doppelganger("p3pt", p3pt)
})

test_that("cat_plot does intervals w/ 3-way interactions (point)", {
  p3pti <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                    interval = TRUE)
  expect_doppelganger("p3pti", p3pti)
})

test_that("cat_plot does plot.points w/ 3-way interactions (point)", {
  p3ptpp <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                     interval = TRUE, plot.points = TRUE, jitter = 0)
  expect_doppelganger("p3ptpp", p3ptpp)
})

test_that("cat_plot does point.shape w/ 3-way interactions (point)", {
  p3ptps <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                     interval = TRUE, plot.points = TRUE, point.shape = TRUE,
                     jitter = 0)
  expect_doppelganger("p3ptps", p3ptps)
})

context("cat_plot no interaction")

test_that("cat_plot handles simple plot w/ no mod. (bar)", {
  p0bar <- cat_plot(fit, pred = color, geom = "bar")
  expect_doppelganger("p0bar", p0bar)
})

test_that("cat_plot handles intervals w/ no mod. (bar)", {
  p0bari <- cat_plot(fit, pred = color, interval = TRUE, geom = "bar")
  expect_doppelganger("p0bari", p0bari)
})

test_that("cat_plot handles plotted points w/ no mod. (bar)", {
  p0barpp <- cat_plot(fit, pred = color, interval = TRUE, plot.points = TRUE,
                      geom = "bar", jitter = 0)
  expect_doppelganger("p0barpp", p0barpp)
})

test_that("cat_plot handles simple plot w/ no mod. (point)", {
  p0pt <- cat_plot(fit, pred = color, geom = "point")
  expect_doppelganger("p0pt", p0pt)
})

test_that("cat_plot handles intervals w/ no mod. (point)", {
  p0pti <- cat_plot(fit, pred = color, interval = TRUE, geom = "point")
  expect_doppelganger("p0pti", p0pti)
})

test_that("cat_plot handles plotted points w/ no mod. (point)", {
  p0ptpp <- cat_plot(fit, pred = color, interval = TRUE, plot.points = TRUE,
                     geom = "point", jitter = 0)
  expect_doppelganger("p0ptpp", p0ptpp)
})

test_that("cat_plot handles point.shape w/ no mod. (point)", {
  p0ptps <- cat_plot(fit, pred = color, interval = TRUE, plot.points = TRUE,
                     geom = "point", point.shape = TRUE, jitter = 0)
  expect_doppelganger("p0ptps", p0ptps)
})

context("brmsfit plots")

test_that("brmsfit objects are supported", {
  skip_if_not_installed("brms")
  bfit <- readRDS("brmfit.rds")
  pcatbfit <- cat_plot(bfit, pred = "Trt", interval = TRUE)
  expect_doppelganger("pcatbfit", pcatbfit)
})

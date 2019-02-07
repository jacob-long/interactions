context("cat_plot lm")

device <- getOption("device")
options(device = "pdf")

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
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE, geom = "line", point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE, geom = "line", point.shape = TRUE,
                              vary.lty = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                              plot.points = TRUE, geom = "point",
                              point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (boxplot)", {
  expect_error(p <- cat_plot(fit, pred = color, modx = cut, geom = "boxplot"))
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
  expect_s3_class(p <- cat_plot(pmod, pred = talent), "gg")
  expect_silent(print(p))
})

context("cat_plot survey")

if (requireNamespace("survey")) {
  library(survey, quietly = TRUE)
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
  test_that("cat_plot handles svyglm", {
    expect_silent(p <- cat_plot(regmodel, pred = both))
    expect_silent(print(p))
  })
}

context("cat_plot merMod")

test_that("cat_plot handles merMod", {
  expect_silent(p <- cat_plot(mv, pred = mode, modx = Gender, interval = FALSE))
  expect_silent(print(p))
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
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                              interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                              interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
                              interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                              interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                              interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                              interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does vary.lty w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                              interval = TRUE, plot.points = TRUE, point.shape = TRUE,
                              vary.lty = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                              interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                              interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
                              interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
                              plot.points = TRUE))
  expect_silent(print(p))
})

context("cat_plot no interaction")

test_that("cat_plot handles simple plot w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                              plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                              geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                              plot.points = TRUE, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                              plot.points = TRUE, geom = "point",
                              point.shape = TRUE))
  expect_silent(print(p))
})

if (requireNamespace("brms")) {
  context("brmsfit plots")
  bfit <- readRDS("brmfit.rds")
  test_that("brmsfit objects are supported", {
    expect_silent(print(cat_plot(bfit, pred = "Trt",
                                 interval = TRUE)))
  })
}

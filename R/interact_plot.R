#' Plot interaction effects in regression models
#'
#' \code{interact_plot} plots regression lines at user-specified levels of a
#'  moderator variable to explore interactions. The plotting is done with
#'  \code{ggplot2} rather than base graphics, which some similar functions use.
#'
#' @param model A regression model. The function is tested with \code{lm},
#'   \code{glm}, \code{\link[survey]{svyglm}}, \code{\link[lme4]{merMod}},
#'   \code{\link[quantreg]{rq}}, \code{\link[brms]{brmsfit}},
#'   \code{stanreg} models.
#'   Models from other classes may work as well but are not officially
#'   supported. The model should include the interaction of interest.
#'
#' @param pred The name of the predictor variable involved
#'  in the interaction. This can be a bare name or string.
#'
#' @param modx The name of the moderator variable involved
#'  in the interaction. This can be a bare name or string.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction. This can be a bare name or string.
#'
#' @param modx.values For which values of the moderator should lines be plotted?
#'   Default is \code{NULL}. If \code{NULL}, then the customary +/- 1 standard
#'   deviation from the mean as well as the mean itself are used for continuous
#'   moderators. If \code{"plus-minus"}, plots lines when the moderator is at
#'   +/- 1 standard deviation without the mean. You may also choose `"terciles"`
#'   to split the data into equally-sized groups and choose the point at the
#'   mean of each of those groups.
#'
#'   If the moderator is a factor variable and \code{modx.values} is
#'   \code{NULL}, each level of the factor is included. You may specify
#'   any subset of the factor levels (e.g., `c("Level 1", "Level 3")`) as long
#'   as there is more than 1. The levels will be plotted in the order you
#'   provide them, so this can be used to reorder levels as well.
#'
#' @param mod2.values For which values of the second moderator should the plot
#'   be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modx.values}.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If `"all"`, all non-focal predictors are centered. You
#'   may instead pass a character vector of variables to center. User can
#'   also use "none" to base all predictions on variables set at 0.
#'   The response variable, `pred`, `modx`, and `mod2` variables are never
#'   centered.
#'
#' @param data Optional, default is NULL. You may provide the data used to
#'   fit the model. This can be a better way to get mean values for centering
#'   and can be crucial for models with variable transformations in the formula
#'   (e.g., `log(x)`) or polynomial terms (e.g., `poly(x, 2)`). You will
#'   see a warning if the function detects problems that would likely be
#'   solved by providing the data with this argument and the function will
#'   attempt to retrieve the original data from the global environment.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as
#'   a scatterplot on top of the interaction lines. The color of the dots will
#'   be based on their moderator value.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals around the line using \code{\link[ggplot2]{geom_ribbon}}.
#'
#' @param int.type Type of interval to plot. Options are "confidence" or
#'  "prediction". Default is confidence interval.
#'
#' @param int.width How large should the interval be, relative to the standard
#'   error? The default, .95, corresponds to roughly 1.96 standard errors and
#'   a .05 alpha level for values outside the range. In other words, for a
#'   confidence interval, .95 is analogous to a 95\% confidence interval.
#'
#' @param outcome.scale For nonlinear models (i.e., GLMs), should the outcome
#'   variable be plotted on the link scale (e.g., log odds for logit models) or
#'   the original scale (e.g., predicted probabilities for logit models)? The
#'   default is \code{"response"}, which is the original scale. For the link
#'   scale, which will show straight lines rather than curves, use
#'   \code{"link"}.
#'
#' @param linearity.check For two-way interactions only. If `TRUE`, plots a
#'   pane for each level of the moderator and superimposes a loess smoothed
#'   line (in gray) over the plot. This enables you to see if the effect is
#'   linear through the span of the moderator. See Hainmueller et al. (2016) in
#'   the references for more details on the intuition behind this. It is
#'   recommended that you also set `plot.points = TRUE` and use
#'   `modx.values = "terciles"` with this option.
#'
#' @inheritParams jtools::summ.lm
#'
#' @param vcov Optional. You may supply the variance-covariance matrix of the
#'  coefficients yourself. This is useful if you are using some method for
#'  robust standard error calculation not supported by the \pkg{sandwich}
#'  package.
#'
#' @param set.offset For models with an offset (e.g., Poisson models), sets an
#'   offset for the predicted values. All predicted values will have the same
#'   offset. By default, this is set to 1, which makes the predicted values a
#'   proportion. See details for more about offset support.
#'
#' @param x.label A character object specifying the desired x-axis label. If
#'   \code{NULL}, the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label. If
#'   \code{NULL}, the variable name is used.
#'
#' @param pred.labels A character vector of 2 labels for the predictor if it is
#'   a 2-level factor or a continuous variable with only 2 values. If
#'   \code{NULL}, the default, the factor labels are used.
#'
#' @param modx.labels A character vector of labels for each level of the
#'   moderator values, provided in the same order as the \code{modx.values}
#'   argument. If \code{NULL}, the values themselves are used as labels unless
#'   \code{modx,values} is also \code{NULL}. In that case, "+1 SD" and "-1 SD"
#'   are used.
#'
#' @param mod2.labels A character vector of labels for each level of the 2nd
#'   moderator values, provided in the same order as the \code{mod2.values}
#'   argument. If \code{NULL}, the values themselves are used as labels unless
#'   \code{mod2.values} is also \code{NULL}. In that case, "+1 SD" and "-1 SD"
#'   are used.
#'
#' @param main.title A character object that will be used as an overall title
#'   for the plot. If \code{NULL}, no main title is used.
#'
#' @param legend.main A character object that will be used as the title that
#'   appears above the legend. If \code{NULL}, the name of the moderating
#'   variable is used.
#'
#' @param color.class See [jtools_colors] for details on the types of arguments
#'    accepted. Default is "CUD Bright" for factor
#'    moderators, "Blues" for +/- SD and user-specified \code{modx.values}
#'    values.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.1;
#'   ggplot's default is 1.
#'
#' @param vary.lty Should the resulting plot have different shapes for each
#'   line in addition to colors? Defaults to \code{TRUE}.
#'
#' @param jitter How much should `plot.points` observed values be "jittered"
#'    via [ggplot2::position_jitter()]? When there are many points near each
#'    other, jittering moves them a small amount to keep them from
#'    totally overlapping. In some cases, though, it can add confusion since
#'    it may make points appear to be outside the boundaries of observed
#'    values or cause other visual issues. Default is 0, but try various
#'    small values (e.g., 0.1) and increase as needed if your points are
#'    overlapping too much. If the argument is a vector with two values,
#'    then the first is assumed to be the jitter for width and the second
#'    for the height.
#'
#' @param rug Show a rug plot in the margins? This uses [ggplot2::geom_rug()]
#'    to show the distribution of the predictor (top/bottom) and/or
#'    response variable (left/right) in the original data. Default is
#'    FALSE.
#'
#' @param rug.sides On which sides should rug plots appear? Default is "b",
#'    meaning bottom. "t" and/or "b" show the distribution of the predictor
#'    while "l" and/or "r" show the distribution of the response. "bl" is
#'    a good option to show both the predictor and response.
#'
#' @param point.size What size should be used for observed data when
#'   `plot.points` is TRUE? Default is 2.
#'
#' @param facet.modx Create separate panels for each level of the moderator?
#'   Default is FALSE, except when `linearity.check` is TRUE.
#'
#' @param robust Should robust standard errors be used to find confidence
#'   intervals for supported models? Default is FALSE, but you should specify
#'   the type of sandwich standard errors if you'd like to use them (i.e.,
#'   `"HC0"`, `"HC1"`, and so on). If `TRUE`, defaults to `"HC3"` standard
#'   errors.
#'
#' @param cluster For clustered standard errors, provide the column name of
#'   the cluster variable in the input data frame (as a string). Alternately,
#'   provide a vector of clusters.
#'
#' @param ... extra arguments passed to `make_predictions`
#'
#' @inheritParams cat_plot
#'
#' @details This function provides a means for plotting conditional effects
#'   for the purpose of exploring interactions in regression models.
#'
#'   The function is designed for two and three-way interactions. For
#'   additional terms, the \pkg{effects} package may be better suited to the
#'   task.
#'
#'   This function supports nonlinear and generalized linear models and by
#'   default will plot them on their original scale
#'   (`outcome.scale = "response"`). To plot them on the linear scale,
#'   use "link" for `outcome.scale`.
#'
#'   While mixed effects models from \code{lme4} are supported, only the fixed
#'   effects are plotted. \code{lme4} does not provide confidence intervals,
#'   so they are not supported with this function either.
#'
#'   Note: to use transformed predictors, e.g., \code{log(variable)},
#'   put its name in quotes or backticks in the argument.
#'
#'   \emph{Details on how observed data are split in multi-pane plots}:
#'
#'   If you set `plot.points = TRUE` and request a multi-pane (facetted) plot
#'   either with a second moderator, `linearity.check = TRUE`, or
#'   `facet.modx = TRUE`, the observed
#'   data are split into as many groups as there  are panes and plotted
#'   separately. If the moderator is a factor, then the way this happens will
#'   be very intuitive since it's obvious which values go in which pane. The
#'   rest of this section will address the case of continuous moderators.
#'
#'   My recommendation is that you use `modx.values = "terciles"` or
#'   `mod2.values = "terciles"` when you want to plot observed data on
#'   multi-pane
#'   plots. When you do, the data are split into three approximately
#'   equal-sized groups with the lowest third, middle third, and highest third
#'   of the data split accordingly. You can replicate this procedure using
#'   [Hmisc::cut2()] with `g = 3` from the `Hmisc` package. Sometimes, the
#'   groups will not be equal in size because the number of observations is
#'   not divisible by 3 and/or there are multiple observations with the same
#'   value at one of the cut points.
#'
#'   Otherwise, a more ad hoc procedure is used to split the data. Quantiles
#'   are found for each `mod2.values` or `modx.values` value. These are not the
#'   quantiles used to split the data, however, since we want the plotted lines
#'   to represent the slope at a typical value in the group. The next step,
#'   then, is to take the mean of each pair of neighboring quantiles and use
#'   these as the cut points.
#'
#'   For example, if the `mod2.values` are at the 25th, 50th, and 75th
#'   percentiles
#'   of the distribution of the moderator, the data will be split at the
#'   37.5th and and 62.5th percentiles. When the variable is
#'   normally distributed, this will correspond fairly closely to using
#'   terciles.
#'
#'   \emph{Info about offsets:}
#'
#'   Offsets are partially supported by this function with important
#'   limitations. First of all, only a single offset per model is supported.
#'   Second, it is best in general to specify offsets with the offset argument
#'   of the model fitting function rather than in the formula. You are much
#'   more likely to have success if you provide the data used to fit the model
#'   with the `data` argument.
#'
#'
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @family interaction tools
#'
#' @seealso \code{\link[rockchalk]{plotSlopes}} from \pkg{rockchalk} performs a
#'   similar function, but
#'   with R's base graphics---this function is meant, in part, to emulate
#'   its features.
#'
#'   \code{\link{sim_slopes}} performs a simple slopes analysis with a similar
#'   argument syntax to this function.
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral
#'  Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' Hainmueller, J., Mummolo, J., & Xu, Y. (2016). How much should we trust
#'   estimates from multiplicative interaction models? Simple tools to improve
#'   empirical practice. SSRN Electronic Journal.
#'   \url{https://doi.org/10.2139/ssrn.2739221}
#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder * Illiteracy, data = states)
#' interact_plot(model = fit, pred = Murder, modx = Illiteracy)
#'
#' # Using interval feature
#' fit <- lm(accel ~ mag * dist, data = attenu)
#' interact_plot(fit, pred = mag, modx = dist, interval = TRUE,
#'   int.type = "confidence", int.width = .8)
#'
#' # Using second moderator
#' fit <- lm(Income ~ HSGrad * Murder * Illiteracy, data = states)
#' interact_plot(model = fit, pred = Murder, modx = Illiteracy, mod2 = HSGrad)
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' interact_plot(regmodel, pred = ell, modx = meals)
#' }
#'
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger * mode + (1 | item), data = VerbAgg,
#'             family = binomial,
#'             control = glmerControl("bobyqa"))
#' interact_plot(mv, pred = Anger, modx = mode)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median ecdf quantile
#' @import ggplot2
#' @export interact_plot

interact_plot <- function(model, pred, modx, modx.values = NULL, mod2 = NULL,
                          mod2.values = NULL, centered = "all", data = NULL,
                          plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence", "prediction"),
                          int.width = .95, outcome.scale = "response",
                          linearity.check = FALSE, facet.modx = FALSE,
                          robust = FALSE, cluster = NULL, vcov = NULL,
                          set.offset = 1,
                          x.label = NULL, y.label = NULL,
                          pred.labels = NULL, modx.labels = NULL,
                          mod2.labels = NULL, main.title = NULL,
                          legend.main = NULL, color.class = NULL,
                          line.thickness = 1, vary.lty = TRUE,
                          point.size = 1, point.shape = FALSE,
                          jitter = 0, rug = FALSE, rug.sides = "b", ...) {

  # Capture extra arguments
  dots <- list(...)
  if (length(dots) > 0) {
    if ("modxvals" %in% names(dots)) {
      modx.values <- dots$modxvals
    }
    if ("mod2vals" %in% names(dots)) {
      mod2.values <- dots$mod2vals
    }
  }

  # Evaluate the modx, mod2, pred args
  # This is getting nasty due to my decision to use NSE
  pred <- as.character(deparse(substitute(pred)))
  pred <- gsub("\"", "", pred, fixed = TRUE)
  modx <- as.character(deparse(substitute(modx)))
  modx <- gsub("\"", "", modx, fixed = TRUE)
  mod2 <- as.character(deparse(substitute(mod2)))
  mod2 <- gsub("\"", "", mod2, fixed = TRUE)
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0 | mod2 == "NULL") {
    mod2 <- NULL
  }

  if (any(c(pred, modx, mod2) %in% centered)) {
    warn_wrap("You cannot mean-center the focal predictor or moderators with
              this function.")
    centered <- centered %not% c(pred, modx, mod2)
    if (length(centered) == 0) {centered <- "none"}
  }

  # Defining "global variables" for CRAN
  modxvals2 <- mod2vals2 <- resp <- NULL

  # Change facet.modx to TRUE if linearity.check is TRUE
  if (linearity.check == TRUE) {facet.modx <- TRUE}

  if (is.null(data)) {
    d <- get_data(model, warn = TRUE)
  } else {
    d <- data
  }
  wts <- get_weights(model, d)$weights

  pred_out <- prep_data(model = model, pred = pred, modx = modx,
                        modx.values = modx.values, mod2 = mod2,
                        mod2.values = mod2.values, centered = centered,
                        interval = interval, int.type = int.type,
                        int.width = int.width, outcome.scale = outcome.scale,
                        linearity.check = linearity.check, robust = robust,
                        cluster = cluster, vcov = vcov, set.offset = set.offset,
                        modx.labels = modx.labels, mod2.labels = mod2.labels,
                        facet.modx = facet.modx, d = d,
                        survey = "svyglm" %in% class(model), wts = wts,
                        preds.per.level = 100, ...)

  # These are the variables created in the helper functions
  meta <- attributes(pred_out)
  # This function attaches all those variables to this environment
  lapply(names(meta), function(x, env) {env[[x]] <- meta[[x]]},
              env = environment())

  # Putting these outputs into separate objects
  pm <- pred_out$predicted
  d <- pred_out$original

  # Check for factor predictor
  if (is.factor(d[[pred]])) {
    # I could assume the factor is properly ordered, but that's too risky
    stop("Focal predictor (\"pred\") cannot be a factor. Either",
         " use it as modx, convert it to a numeric dummy variable,",
         " or use the cat_plot function for factor by factor interaction",
         " plots.")
  }

  # Send to internal plotting function
  plot_mod_continuous(predictions = pm, pred = pred, modx = modx, resp = resp,
                      mod2 = mod2, data = d, plot.points = plot.points,
                      interval = interval, linearity.check = linearity.check,
                      x.label = x.label, y.label = y.label,
                      pred.labels = pred.labels, modx.labels = modx.labels,
                      mod2.labels = mod2.labels, main.title = main.title,
                      legend.main = legend.main, color.class = color.class,
                      line.thickness = line.thickness,
                      vary.lty = vary.lty, jitter = jitter,
                      modxvals2 = modxvals2, mod2vals2 = mod2vals2,
                      wts = wts, rug = rug, rug.sides = rug.sides,
                      point.size = point.size, point.shape = point.shape,
                      facet.modx = facet.modx)

}

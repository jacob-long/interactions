
#' Plot interaction effects between categorical predictors.
#'
#' `cat_plot` is a complementary function to [interact_plot()] that is designed
#' for plotting interactions when both predictor and moderator(s) are
#' categorical (or, in R terms, factors).
#'
#' @param pred A categorical predictor variable that will appear on the x-axis.
#' @param modx A categorical moderator variable.
#' @param mod2 For three-way interactions, the second categorical moderator.
#'
#' @param geom What type of plot should this be? There are several options
#'   here since the best way to visualize categorical interactions varies by
#'   context. Here are the options:
#'
#'   * `"point"`: The default. Simply plot the point estimates. You may want to
#'      use `point.shape = TRUE` with this and you should also consider
#'     `interval = TRUE` to visualize uncertainty.
#'
#'   * `"line"`: This connects observations across levels of the `pred`
#'     variable with a line. This is a good option when the `pred` variable
#'     is ordinal (ordered). You may still consider `point.shape = TRUE` and
#'     `interval = TRUE` is still a good idea.
#'
#'   * `"bar"`: A bar chart. Some call this a "dynamite plot."
#'     Many applied researchers advise against this type of plot because it
#'     does not represent the distribution of the observed data or the
#'     uncertainty of the predictions very well. It is best to at least use the
#'     `interval = TRUE` argument with this geom.
#'
#'   * `"boxplot"`: This geom plots a dot and whisker plot. These can be useful
#'     for understanding the distribution of the observed data without having
#'     to plot all the observed points (especially helpful with larger data
#'     sets). **However**, it is important to note the boxplots are not based
#'     on the model whatsoever.
#'
#' @param pred.values Which values of the predictor should be included in the
#'   plot? By default, all levels are included.
#'
#' @param pred.labels A character vector of equal length to the number of
#'   factor levels of the predictor (or number specified in `predvals`). If
#'   \code{NULL}, the default, the factor labels are used.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as a
#'   scatterplot on top of the interaction lines. Note that if
#'   `geom = "bar"`, this will cause the bars to become transparent so you can
#'   see the points.
#'
#' @param point.shape For plotted points---either of observed data or predicted
#'   values with the "point" or "line" geoms---should the shape of the points
#'   vary by the values of the factor? This is especially useful if you aim to
#'   be black and white printing- or colorblind-friendly.
#'
#' @param color.class Any palette argument accepted by
#'   \code{\link[ggplot2]{scale_colour_brewer}}. Default is "Set2".
#'   You may also simply supply a vector of colors accepted by
#'   `ggplot2` and of equal length to the number of moderator levels.
#'
#' @param interval.geom For categorical by categorical interactions.
#'   One of "errorbar" or "linerange". If the former,
#'   [ggplot2::geom_errorbar()] is used. If the latter,
#'   [ggplot2::geom_linerange()] is used.
#'
#' @param geom.alpha What should the alpha aesthetic be for the plotted
#'   lines/bars? Default is NULL, which means it is set depending on the value
#'   of `geom` and `plot.points`.
#'
#' @param dodge.width What should the `width` argument to
#'   [ggplot2::position_dodge()] be? Default is NULL, which means it is set
#'   depending on the value of `geom`.
#'
#' @param errorbar.width How wide should the error bars be? Default is NULL,
#'   meaning it is set depending on the value `geom`. Ignored if `interval`
#'   is FALSE.
#'
#' @param pred.point.size If TRUE and `geom` is `"point"` or `"line"`,
#'  sets the size of the predicted points. Default is 3.5.
#'  Note the distinction from `point.size`, which refers to the
#'  observed data points.
#'
#' @param jitter How much should `plot.points` observed values be "jittered"
#'    via [ggplot2::position_jitter()]? When there are many points near each
#'    other, jittering moves them a small amount to keep them from
#'    totally overlapping. In some cases, though, it can add confusion since
#'    it may make points appear to be outside the boundaries of observed
#'    values or cause other visual issues. Default is 0.1, but increase as
#'    needed if your points are overlapping too much or set to 0 for no jitter.
#'    If the argument is a vector with two values, then the first is assumed to
#'    be the jitter for width and the second for the height.
#'
#' @inheritParams interact_plot
#'
#' @details This function provides a means for plotting conditional effects
#'   for the purpose of exploring interactions in the context of regression.
#'   You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   The function is designed for two and three-way interactions. For
#'   additional terms, the
#'   \code{\link[effects]{effects}} package may be better suited to the task.
#'
#'   This function supports nonlinear and generalized linear models and by
#'   default will plot them on
#'   their original scale (\code{outcome.scale = "response"}).
#'
#'   While mixed effects models from \code{lme4} are supported, only the fixed
#'   effects are plotted. \code{lme4} does not provide confidence intervals,
#'   so they are not supported with this function either.
#'
#'   Note: to use transformed predictors, e.g., \code{log(variable)},
#'   provide only the variable name to `pred`, `modx`, or `mod2` and supply
#'   the original data separately to the `data` argument.
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
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like a user-created plot and expanded upon as such.
#'
#' @family interaction tools
#'
#' @examples
#'
#' library(ggplot2)
#' fit <- lm(price ~ cut * color, data = diamonds)
#' cat_plot(fit, pred = color, modx = cut, interval = TRUE)
#'
#' # 3-way interaction
#'
#' ## Will first create a couple dichotomous factors to ensure full rank
#' mpg2 <- mpg
#' mpg2$auto <- "auto"
#' mpg2$auto[mpg2$trans %in% c("manual(m5)", "manual(m6)")] <- "manual"
#' mpg2$auto <- factor(mpg2$auto)
#' mpg2$fwd <- "2wd"
#' mpg2$fwd[mpg2$drv == "4"] <- "4wd"
#' mpg2$fwd <- factor(mpg2$fwd)
#' ## Drop the two cars with 5 cylinders (rest are 4, 6, or 8)
#' mpg2 <- mpg2[mpg2$cyl != "5",]
#' mpg2$cyl <- factor(mpg2$cyl)
#' ## Fit the model
#' fit3 <- lm(cty ~ cyl * fwd * auto, data = mpg2)
#'
#' # The line geom looks good for an ordered factor predictor
#' cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
#'  interval = TRUE)
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median
#' @export cat_plot
#' @import ggplot2
#'

cat_plot <- function(model, pred, modx = NULL, mod2 = NULL,
  data = NULL, geom = c("point", "line", "bar", "boxplot"), pred.values = NULL,
  modx.values = NULL, mod2.values = NULL, interval = TRUE, plot.points = FALSE,
  point.shape = FALSE, vary.lty = FALSE, centered = "all",
  int.type = c("confidence", "prediction"), int.width = .95, line.thickness = 1,
  point.size = 1.5, pred.point.size = 3.5, jitter = 0.1, geom.alpha = NULL,
  dodge.width = NULL, errorbar.width = NULL,
  interval.geom = c("errorbar", "linerange"), outcome.scale = "response",
  robust = FALSE, cluster = NULL, vcov = NULL, pred.labels = NULL,
  modx.labels = NULL, mod2.labels = NULL, set.offset = 1, x.label = NULL,
  y.label = NULL, main.title = NULL, legend.main = NULL,
  color.class = "CUD Bright", ...) {

  # Capture extra arguments
  dots <- list(...)
  if (length(dots) > 0) {
    if ("predvals" %in% names(dots)) {
      pred.values <- dots$predvals
    }
    if ("modxvals" %in% names(dots)) {
      modx.values <- dots$modxvals
    }
    if ("mod2vals" %in% names(dots)) {
      mod2.values <- dots$mod2vals
    }
  }

  # Evaluate the modx, mod2, pred args
  pred <- as.character(deparse(substitute(pred)))
  pred <- gsub("\"", "", pred, fixed = TRUE)
  modx <- as.character(deparse(substitute(modx)))
  modx <- gsub("\"", "", modx, fixed = TRUE)
  # To avoid unexpected behavior, need to un-un-parse modx when it is NULL
  if (length(modx) == 0 | modx == "NULL") {
    modx <- NULL
  }
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

  # Get the geom if not specified
  geom <- geom[1]
  if (geom == "dot") {geom <- "point"}

  # Defining "global variables" for CRAN
  modxvals2 <- mod2vals2 <- resp <- NULL

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
                        linearity.check = FALSE, robust = robust,
                        cluster = cluster, vcov = vcov, set.offset = set.offset,
                        modx.labels = modx.labels, mod2.labels = mod2.labels,
                        facet.modx = FALSE, d = d,
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

  plot_cat(predictions = pm, pred = pred, modx = modx, mod2 = mod2,
           data = d, geom = geom, pred.values = pred.values,
           modx.values = modx.values, mod2.values = mod2.values
           , interval = interval,
           plot.points = plot.points,
           point.shape = point.shape, vary.lty = vary.lty,
           pred.labels = pred.labels, modx.labels = modx.labels,
           mod2.labels = mod2.labels, x.label = x.label, y.label = y.label,
           main.title = main.title, legend.main = legend.main,
           color.class = color.class, wts = wts, resp = resp,
           geom.alpha = geom.alpha, dodge.width = dodge.width,
           errorbar.width = errorbar.width, interval.geom = interval.geom,
           point.size = point.size, line.thickness = line.thickness,
           pred.point.size = pred.point.size, jitter = jitter)

}

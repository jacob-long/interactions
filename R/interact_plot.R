#' Plot interaction effects in regression models
#'
#' \code{interact_plot} plots regression lines at user-specified levels of a
#'  moderator variable to explore interactions. The plotting is done with
#'  \code{ggplot2} rather than base graphics, which some similar functions use.
#'
#' @param model A regression model. The function is tested with \code{lm},
#'   \code{glm}, \code{\link[survey]{svyglm}}, \code{\link[lme4]{merMod}},
#'   `rq` (from `quantreg`), \code{\link[brms]{brmsfit}},
#'   \code{stanreg} models.
#'   Models from other classes may work as well but are not officially
#'   supported. The model should include the interaction of interest.
#'
#' @param pred The name of the predictor variable involved
#'  in the interaction. This can be a bare name or string. Note that it
#'  is evaluated using `rlang`, so programmers can use the `!!` syntax
#'  to pass variables instead of the verbatim names.
#'
#' @param modx The name of the moderator variable involved
#'  in the interaction. This can be a bare name or string. The same
#'  `rlang` proviso applies as with `pred`.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction. This can be a bare name or string.
#'  The same `rlang` proviso applies as with `pred`.
#'
#' @param modx.values For which values of the moderator should lines be
#'   plotted? There are two basic options:
#'
#'   * A vector of values (e.g., `c(1, 2, 3)`)
#'   * A single argument asking to calculate a set of values. See details
#'   below.
#'
#'   Default is \code{NULL}. If \code{NULL} (or `mean-plus-minus`),
#'   then the customary +/- 1 standard
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
#'   confidence interval, .95 is analogous to a 95% confidence interval.
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
#' @param colors See [`jtools_colors`][jtools::jtools_colors] for details on the
#'    types of arguments accepted. Default is "CUD Bright" for factor
#'    moderators, "Blues" for +/- SD and user-specified \code{modx.values}
#'    values.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.
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
#'   `plot.points` is TRUE? Default is 1.5.
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
#' @inheritParams jtools::effect_plot
#'
#' @details This function provides a means for plotting conditional effects
#'   for the purpose of exploring interactions in regression models.
#'
#'   The function is designed for two and three-way interactions. For
#'   additional terms, the `effects` package may be better suited to the
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
#'   `cut2()` with `g = 3` from the `Hmisc` package. Sometimes, the
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
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @seealso \code{plotSlopes} from \code{rockchalk} performs a
#'   similar function, but
#'   with R's base graphics---this function is meant, in part, to emulate
#'   its features.
#' 
#'   Functions from the `margins` and `sjPlot` packages may also be useful
#'   if this one isn't working for you.
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
#'  \doi{10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' Hainmueller, J., Mummolo, J., & Xu, Y. (2016). How much should we trust
#'   estimates from multiplicative interaction models? Simple tools to improve
#'   empirical practice. SSRN Electronic Journal.
#'   \doi{10.2139/ssrn.2739221}
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
#' @import rlang
#' @export interact_plot

interact_plot <- function(model, pred, modx, modx.values = NULL, mod2 = NULL,
                          mod2.values = NULL, centered = "all", data = NULL,
                          at = NULL,
                          plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence", "prediction"),
                          int.width = .95, outcome.scale = "response",
                          linearity.check = FALSE, facet.modx = FALSE,
                          robust = FALSE, cluster = NULL, vcov = NULL,
                          set.offset = 1,
                          x.label = NULL, y.label = NULL,
                          pred.labels = NULL, modx.labels = NULL,
                          mod2.labels = NULL, main.title = NULL,
                          legend.main = NULL, colors = NULL,
                          line.thickness = 1, vary.lty = TRUE,
                          point.size = 1.5, point.shape = FALSE,
                          jitter = 0, rug = FALSE, rug.sides = "b",
                          partial.residuals = FALSE, point.alpha = 0.6,
                          color.class = NULL,  ...) {

  # Capture extra arguments
  dots <- list(...)
  if (length(dots) > 0) {
    if ("modxvals" %in% names(dots)) {
      modx.values <- dots$modxvals
    }
    if ("mod2vals" %in% names(dots)) {
      mod2.values <- dots$mod2vals
    }
    # If it's a categorical predictor, I want a different default for the
    # geom argument than cat_plot() uses so it looks more like what you'd 
    # expect from this function
    if ("geom" %nin% names(dots)) {
      geom <- "line"
    } else {
      geom <- dots$geom
    }
  } else {
    geom <- "line"
  }

  if (!is.null(color.class)) {
    colors <- color.class
    msg_wrap("The color.class argument is deprecated. Please use 'colors'
             instead.")
  }

  # Evaluate the modx, mod2, pred args
  pred <- as_name(enquo(pred))
  modx <- enquo(modx)
  modx <- if (quo_is_null(modx)) {NULL} else {as_name(modx)}
  mod2 <- enquo(mod2)
  mod2 <- if (quo_is_null(mod2)) {NULL} else {as_name(mod2)}

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
    d <- get_data(model, warn = TRUE, ...)
  } else {
    d <- data
  }
  weights <- get_weights(model, d)$weights_name

  # Check for variables in the data
  if (any(c(pred, modx, mod2) %nin% names(d))) {
    missed_vars <- c(pred, modx, mod2) %not% names(d)
    stop_wrap(paste(missed_vars, collapse = " and "),
              ifelse(length(missed_vars) > 1, yes = " were ", no = " was "),
              "not found in the data. If you are using a transformed variable,
              like 'log(x)', use the non-transformed name ('x') as the input to
              this function.")
  }

  # If modx.values is named, use the names as labels
  if (is.null(modx.labels) & !is.null(names(modx.values))) {
    modx.labels <- names(modx.values)
  }
  # If mod2.values is named, use the names as labels
  if (is.null(mod2.labels) & !is.null(names(mod2.values))) {
    mod2.labels <- names(mod2.values)
  }

  pred_out <- prep_data(model = model, pred = pred, modx = modx,
                        modx.values = modx.values, mod2 = mod2,
                        mod2.values = mod2.values, centered = centered,
                        interval = interval, int.type = int.type,
                        int.width = int.width, outcome.scale = outcome.scale,
                        linearity.check = linearity.check, robust = robust,
                        cluster = cluster, vcov = vcov, set.offset = set.offset,
                        modx.labels = modx.labels, mod2.labels = mod2.labels,
                        facet.modx = facet.modx, d = d,
                        survey = "svyglm" %in% class(model), weights = weights,
                        preds.per.level = 100,
                        partial.residuals = partial.residuals, at = at, ...)

  # These are the variables created in the helper functions
  meta <- attributes(pred_out)
  # This function attaches all those variables to this environment
  lapply(names(meta), function(x, env) {env[[x]] <- meta[[x]]},
              env = environment())

  # Putting these outputs into separate objects
  pm <- pred_out$predicted
  d <- pred_out$original

  # Check for factor predictor and send to plot_cat() if so
  if (!is.numeric(d[[pred]])) {
    # Warn users that this is kinda janky
    cli::cli_inform(c(
      x = "Detected factor predictor.",
      i = "Plotting with cat_plot() instead.",
      i = "See {.help interactions::cat_plot} for full details on 
         how to specify models with categorical predictors.",
      i = "If you  experience errors or unexpected results, try using 
           cat_plot() directly."
    ))
    # Gather arguments for plot_cat()
    args <- list(predictions = pm, pred = pred, modx = modx, mod2 = mod2,
                  data = d, modx.values = modxvals2, mod2.values = mod2vals2,
                  interval = interval,
                  plot.points = plot.points | partial.residuals,
                  point.shape = point.shape, vary.lty = vary.lty,
                  pred.labels = pred.labels, modx.labels = modx.labels,
                  mod2.labels = mod2.labels, x.label = x.label, y.label = y.label,
                  main.title = main.title, legend.main = legend.main,
                  colors = colors, weights = weights, resp = resp,
                  point.size = point.size, line.thickness = line.thickness,
                  jitter = jitter, point.alpha = point.alpha, geom = geom)
    # Deal with cat_plot() arguments provided via ...
    if (length(dots) > 0) {
      # Make sure it's not geom which I've already handled
      if (length(dots %not% "geom") > 0) {
        # Append to this list
        args <- c(args, dots %not% "geom")
      }
    }
    # Call internal plotting function
    return(do.call("plot_cat", args))
    # Using plot_cat turns out to be more robust than cat_plot. I'm doing 
    # something dumb and/or badly with environments that makes calling 
    # plot_cat() within a function fail inside of testthat (and inside of
    # any arbitrary function that calls interact_plot() with a categorical
    # predictor even in the "normal" environment).
  } else {
    # Send to internal plotting function
    plot_mod_continuous(predictions = pm, pred = pred, modx = modx, resp = resp,
                        mod2 = mod2, data = d,
                        plot.points = plot.points | partial.residuals,
                        interval = interval, linearity.check = linearity.check,
                        x.label = x.label, y.label = y.label,
                        pred.labels = pred.labels, modx.labels = modx.labels,
                        mod2.labels = mod2.labels, main.title = main.title,
                        legend.main = legend.main, colors = colors,
                        line.thickness = line.thickness,
                        vary.lty = vary.lty, jitter = jitter,
                        modxvals2 = modxvals2, mod2vals2 = mod2vals2,
                        weights = weights, rug = rug, rug.sides = rug.sides,
                        point.size = point.size, point.shape = point.shape,
                        facet.modx = facet.modx, point.alpha = point.alpha)
  }

}

# Workhorse plotting function
plot_mod_continuous <- function(predictions, pred, modx, resp, mod2 = NULL,
                                data = NULL, plot.points = FALSE,
                                interval = FALSE, linearity.check = FALSE,
                                x.label = NULL, y.label = NULL,
                                pred.labels = NULL, modx.labels = NULL,
                                mod2.labels = NULL, main.title = NULL,
                                legend.main = NULL, colors = NULL,
                                line.thickness = 1.1, vary.lty = TRUE,
                                jitter = 0, modxvals2 = NULL,
                                mod2vals2 = NULL, weights = NULL, rug = FALSE,
                                rug.sides = "b",
                                point.shape = FALSE, point.size = 2,
                                facet.modx = FALSE, point.alpha = 0.6) {

  d <- data
  pm <- predictions

  # Setting default for colors
  if (is.null(colors) && (facet.modx == TRUE | linearity.check == TRUE)) {
    colors <- rep("black", times = length(modxvals2))
    vary.lty <- FALSE
    point.shape <- FALSE
  }
  if (is.factor(d[[modx]])) {
    facmod <- TRUE
    if (is.null(colors)) {
      colors <- "CUD Bright"
    }
  } else {
    facmod <- FALSE
    if (is.null(colors)) {
      colors <- "blue"
    }
  }

  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}

  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)) {
    legend.main <- modx
  }

  if (is.null(x.label)) {
    x.label <- pred
  }

  if (is.null(y.label)) {
    y.label <- resp
  }

  if (is.null(modxvals2)) {
    modxvals2 <- unique(pm[[modx]])
  }

  if (!is.null(mod2) && is.null(mod2vals2)) {
    mod2vals2 <- unique(pm[[mod2]])
  }

  gradient <- is.numeric(d[[modx]])

  # Checking if user provided the colors his/herself
  colors <- suppressWarnings(get_colors(colors, length(modx.labels),
                                        gradient = gradient))

  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash",
             "12223242", "F282", "F4448444", "224282F2", "F1")
  ltypes <- types[seq_along(modxvals2)]

  # Reverse the order of the linetypes to make thick line go to biggest value
  if (is.numeric(modxvals2) & all(sort(modxvals2) == modxvals2)) {
    ltypes <- rev(ltypes)
  } else if (!is.null(mod2) & !(is.numeric(modxvals2) & !all(sort(modxvals2) == modxvals2))) { # also flip for factor second moderators
    ltypes <- rev(ltypes)
  }

  if (gradient == FALSE) {
    names(colors) <- modx.labels
  }
  names(ltypes) <- modx.labels

  # Prepare names for tidy evaluation
  pred <- sym(pred)
  resp <- sym(resp)
  if (!is.null(modx)) {modx <- sym(modx)}
  if (!is.null(mod2)) {mod2 <- sym(mod2)}
  if (!is.null(weights)) {weights <- sym(weights)}

  lty <- if (vary.lty) sym("modx_group") else NULL
  # Don't use 'modx_group' if I don't have to since it makes it harder for
  # users to make changes after the fact
  grp <- if (vary.lty | facet.modx) sym("modx_group") else modx

  p <- ggplot(pm, aes(x = !! pred, y = !! resp, colour = !! modx,
                      group = !! grp, linetype = !! lty))

  p <- p + geom_path(linewidth = line.thickness, show.legend = !facet.modx)

  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm,
                         aes(x = !! pred, ymin = !! sym("ymin"),
                             ymax = !! sym("ymax"), fill = !! modx,
                             group = !! grp, colour = !! modx, linetype = NA),
                         alpha = 1/5, show.legend = FALSE,
                         inherit.aes = TRUE)
  }

  # If third mod, facet by third mod
  facet_form <- "~"
  modgroup <- NULL
  # First, decide whether we're faceting at all
  if (!is.null(mod2) || facet.modx == TRUE) {
    do_facets <- TRUE
  } else {do_facets <- FALSE}
  # If faceting by modx, add that to formula
  if (linearity.check == TRUE | facet.modx == TRUE) {
    facet_form <- paste(facet_form, "modx_group")
    modgroup <- "modx_group"
  }
  # If faceting by mod2, add that to formula
  if (!is.null(mod2)) {
    facet_form <- paste(facet_form,
                        ifelse(facet_form == "~", yes = "mod2_group",
                               no = "+ mod2_group"))
    if (!is.null(modgroup)) {
      modgroup <- "modgroup"
    } else {
      modgroup <- "mod2group"
    }
  }

  if (do_facets == TRUE) {
    if (!is.null(mod2) & (linearity.check == TRUE | facet.modx == TRUE)) {
      num_unique <- nrow(unique(pm[c("modx_group", "mod2_group")]))
      if (num_unique %in% c(3, 6, 9)) {
        # 1 x 3, 2 x 3, or (most commonly) 3 x 3
        num_cols <- 3
      } else if (num_unique %in% c(4)) {
        # 2 x 2
        num_cols <- 2
      } else { # let ggplot2 decide
        num_cols <- NULL
      }
    } else {num_cols <- NULL}
    p <- p + facet_wrap(as.formula(facet_form), ncol = num_cols)
  }

  if (linearity.check == TRUE) {
    p <- p + stat_smooth(data = d,
                         aes(x = !! pred, y = !! resp, group = !! grp),
                         method = "loess", linewidth = 1,
                         show.legend = FALSE, inherit.aes = FALSE,
                         se = FALSE, span = 2, geom = "line",
                         alpha = 0.6, color = "red")
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {

    if (!is.numeric(d[[as_string(modx)]]) & point.shape) {
      shape_arg <- modx
      # Only show legend if there's a shape aesthetic
      show_legend <- TRUE
    } else {
      shape_arg <- NULL
      show_legend <- FALSE
    }
    constants <- list(alpha = point.alpha)
    if (is.null(weights)) {
      # Only use constant size if weights are not used
      constants$size <- point.size
    }
    # Need to use layer function to programmatically define constant aesthetics
    p <- p + layer(geom = "point", data = d, stat = "identity",
                   inherit.aes = TRUE, show.legend = show_legend,
                   mapping = aes(x = !! pred, y = !! resp, size = !! weights,
                                 group = !! grp, colour = !! modx,
                                 shape = !! shape_arg),
                   position = position_jitter(width = jitter[1],
                                              height = jitter[2]),
                   params = constants) +
      scale_size(range = c(1 * point.size, 5 * point.size), guide = "none")

  }

  # Rug plot for marginal distributions
  if (rug == TRUE) {
    p <- p + geom_rug(data = d, aes(linetype = NULL),
                      alpha = 0.6,
                      position = position_jitter(width = jitter[1],
                                                 height = jitter[2]),
                      sides = rug.sides, inherit.aes = TRUE)
  }

  # Using theme_apa for theming...but using legend title and side positioning
  if (is.null(mod2)) {
    p <- p + theme_nice(legend.pos = "right")
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_nice(legend.pos = "bottom")
  }
  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Getting rid of tick marks for factor predictor
  if (length(unique(d[[pred]])) == 2) {
    # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[[pred]]), decreasing = F)
    if (is.null(pred.labels)) {
      p <- p + scale_x_continuous(breaks = brks)
    } else {
      if (length(pred.labels) == 2) { # Make sure pred.labels has right length
        p <- p + scale_x_continuous(breaks = brks, labels = pred.labels)
      } else {
        warning("pred.labels argument has the wrong length. It won't be used")
        p <- p + scale_x_continuous(breaks = brks)
      }
    }
  }

  # Get scale colors, provide better legend title
  if (!is.numeric(d[[as_string(modx)]])) {
    p <- p + scale_colour_manual(name = legend.main, values = colors,
                                 breaks = names(colors),
                                 aesthetics = c("colour", "fill"))
  } else {
    limits <- quant(d[[modx]], probs = c(.1, .9))
    if (min2(modxvals2) < limits[1]) {limits[1] <- min2(modxvals2)}
    if (max2(modxvals2) > limits[2]) {limits[2] <- max2(modxvals2)}
    p <- p + scale_colour_gradientn(name = legend.main,
                                    breaks = modxvals2,
                                    labels = modx.labels,
                                    colors = colors,
                                    limits = limits,
                                    oob = squish,
                                    aesthetics = c("colour", "fill"),
                                    guide = "legend")
  }

  if (vary.lty == TRUE) { # Add line-specific changes
    p <- p + scale_linetype_manual(name = legend.main, values = ltypes,
                                   breaks = names(ltypes),
                                   na.value = "blank")
    # Need some extra width to show the linetype pattern fully
    p <- p + theme(legend.key.width = grid::unit(3, "lines"))
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  return(p)

}

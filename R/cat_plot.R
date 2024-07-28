
#' Plot interaction effects between categorical predictors.
#'
#' `cat_plot` is a complementary function to [interact_plot()] that is designed
#' for plotting interactions when both predictor and moderator(s) are
#' categorical (or, in R terms, factors).
#'
#' @param pred A categorical predictor variable that will appear on the x-axis.
#'  Note that it is evaluated using `rlang`, so programmers can use the `!!`
#'  syntax to pass variables instead of the verbatim names.
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
#' @param colors Any palette argument accepted by [jtools::get_colors()]. 
#'   Default is "CUD Bright" for factor moderators and "blue" for continuous 
#'   moderators. You may also simply supply a vector of colors accepted by
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
#' @inheritParams jtools::effect_plot
#'
#' @details This function provides a means for plotting conditional effects
#'   for the purpose of exploring interactions in the context of regression.
#'   You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   The function is designed for two and three-way interactions. For
#'   additional terms, the
#'   \code{effects} package may be better suited to the task.
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
  data = NULL, geom = c("point", "line", "bar"), pred.values = NULL,
  modx.values = NULL, mod2.values = NULL, interval = TRUE, plot.points = FALSE,
  point.shape = FALSE, vary.lty = FALSE, centered = "all",
  int.type = c("confidence", "prediction"), int.width = .95,
  line.thickness = 1.1,
  point.size = 1.5, pred.point.size = 3.5, jitter = 0.1, geom.alpha = NULL,
  dodge.width = NULL, errorbar.width = NULL,
  interval.geom = c("errorbar", "linerange"), outcome.scale = "response",
  robust = FALSE, cluster = NULL, vcov = NULL, pred.labels = NULL,
  modx.labels = NULL, mod2.labels = NULL, set.offset = 1, x.label = NULL,
  y.label = NULL, main.title = NULL, legend.main = NULL,
  colors = NULL, partial.residuals = FALSE, point.alpha = 0.6,
  color.class = NULL, at = NULL, ...) {

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

  if (!is.null(color.class)) {
    colors <- color.class
    msg_wrap("The color.class argument is deprecated. Please use 'colors'
             instead.")
  }

  # Evaluate the modx, mod2, pred args
  pred <- quo_name(enexpr(pred))
  modx <- quo_name(enexpr(modx))
  if (modx == "NULL") {modx <- NULL}
  mod2 <- quo_name(enexpr(mod2))
  if (mod2 == "NULL") {mod2 <- NULL}

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
  weights <- get_weights(model, d)$weights_name

  # If modx.values is named, use the names as labels
  if (is.null(modx.labels) & !is.null(names(modx.values))) {
    modx.labels <- names(modx.values)
  }
  # If mod2.values is named, use the names as labels
  if (is.null(mod2.labels) & !is.null(names(mod2.values))) {
    mod2.labels <- names(mod2.values)
  }

  pred_out <- prep_data(model = model, pred = pred, modx = modx,
                        pred.values = pred.values,
                        modx.values = modx.values, mod2 = mod2,
                        mod2.values = mod2.values, centered = centered,
                        interval = interval, int.type = int.type,
                        int.width = int.width, outcome.scale = outcome.scale,
                        linearity.check = FALSE, robust = robust,
                        cluster = cluster, vcov = vcov, set.offset = set.offset,
                        pred.labels = pred.labels,
                        modx.labels = modx.labels, mod2.labels = mod2.labels,
                        facet.modx = FALSE, d = d,
                        survey = inherits(model, "svyglm"), weights = weights,
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

  plot_cat(predictions = pm, pred = pred, modx = modx, mod2 = mod2,
           data = d, geom = geom, pred.values = pred.values,
           modx.values = modx.values, mod2.values = mod2.values,
           interval = interval,
           plot.points = plot.points | partial.residuals,
           point.shape = point.shape, vary.lty = vary.lty,
           pred.labels = pred.labels, modx.labels = modx.labels,
           mod2.labels = mod2.labels, x.label = x.label, y.label = y.label,
           main.title = main.title, legend.main = legend.main,
           colors = colors, weights = weights, resp = resp,
           geom.alpha = geom.alpha, dodge.width = dodge.width,
           errorbar.width = errorbar.width, interval.geom = interval.geom,
           point.size = point.size, line.thickness = line.thickness,
           pred.point.size = pred.point.size, jitter = jitter,
           point.alpha = point.alpha)

}

# Workhorse plotter
plot_cat <- function(predictions, pred, modx = NULL, mod2 = NULL,
                     data = NULL, geom = c("point", "line", "bar", "boxplot"),
                     pred.values = NULL,
                     modx.values = NULL, mod2.values = NULL, interval = TRUE,
                     plot.points = FALSE,
                     point.shape = FALSE, vary.lty = FALSE,  pred.labels = NULL,
                     modx.labels = NULL, mod2.labels = NULL, x.label = NULL,
                     y.label = NULL, main.title = NULL, legend.main = NULL,
                     colors = "CUD Bright", weights = NULL, resp = NULL,
                     jitter = 0.1, geom.alpha = NULL, dodge.width = NULL,
                     errorbar.width = NULL,
                     interval.geom = c("errorbar", "linerange"),
                     line.thickness = 1.1, point.size = 1,
                     pred.point.size = 3.5, point.alpha = 0.6) {

  pm <- predictions
  d <- data

  geom <- geom[1]
  if (geom == "dot") {geom <- "point"}
  if (geom %in% c("boxplot", "box")) {
    stop_wrap("The boxplot geom is no longer supported.")
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

  # Deal with numeric predictors coerced into factors
  if (is.numeric(pm[[pred]])) {
    pred.levels <- if (!is.null(pred.values)) {pred.values} else {
      unique(pm[[pred]])
    }
    pred.labels <- if (!is.null(pred.labels)) {pred.labels} else {
      unique(pm[[pred]])
    }
    pm[[pred]] <- factor(pm[[pred]], levels = pred.levels,
                         labels = pred.labels)

    # Make sure only observations of requested levels of predictor are included
    d <- d[d[[pred]] %in% pred.levels,]
    d[[pred]] <- factor(d[[pred]], levels = pred.levels, labels = pred.labels)
  }

  # Create an indicator for my frequent queries about whether modx is 
  # continuous
  numeric_modx <- FALSE
  if (!is.null(modx)) {
    numeric_modx <- is.numeric(d[[modx]])
  }
  
  # Prepare names for tidy evaluation
  pred <- sym(pred)
  resp <- sym(resp)
  if (!is.null(modx)) {modx <- sym(modx)}
  if (!is.null(mod2)) {mod2 <- sym(mod2)}
  if (!is.null(weights)) {weights <- sym(weights)}

  # Checking if user provided the colors
  if (is.null(colors)) {colors <- if (!numeric_modx) "CUD Bright" else "blue"}
  # Retrieve colors
  colors <- suppressWarnings(get_colors(colors, length(modx.labels),
                             gradient = numeric_modx))

  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash",
             "12223242", "F282", "F4448444", "224282F2", "F1")
  ltypes <- types[seq_along(modx.labels)]

  # use named vectors to keep these aesthetics and their labels together
  names(ltypes) <- modx.labels
  if (!numeric_modx) {
    names(colors) <- modx.labels
  }

  # Specify a sensible default transparency for the geom depending on what it is
  if (is.null(geom.alpha)) {
    a_level <- 1
    if (plot.points == TRUE) {
      if (!is.null(modx)) {
        a_level <- 0
      } else {
        a_level <- 0.5
      }
    } else if (interval == TRUE) {
      a_level <- 0.5
    }
  } else {a_level <- geom.alpha}

  # Set a sensible default dodge depending on what the geom is
  if (is.null(dodge.width)) {
    dodge.width <- if (geom %in% c("bar", "point")) {0.9} else {0}
  }
  # Set a sensible default errorbar width depending on what the geom is
  if (is.null(errorbar.width)) {
    errorbar.width <- if (geom == "point") {
      0.9
    } else if (geom == "bar") {
      0.75
    } else {0.5}
  }

  # Can't have point shapes if modx is numeric
  if (numeric_modx) {
    point.shape <- FALSE
  }

  # Create arguments for ggplot calls
  shape <- if (point.shape == TRUE) {modx} else {NULL}
  fill <- if (point.shape == TRUE) {modx} else {NULL}
  lty <- if (vary.lty == TRUE) {if (numeric_modx) sym("modx_group") else modx} else {NULL}
  grp <- if (!is.null(modx)) modx else 1

  # Make base plot object
  p <- ggplot(pm, aes(x = !! pred, y = !! resp, group = !! grp,
                      colour = !! modx, fill = !! modx,
                      shape = !! shape, linetype = !! lty))

  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level)
  } else if (geom %in% c("point", "line")) {
    p <- p + geom_point(size = pred.point.size,
                        position = position_dodge(dodge.width))
  }

  if (geom == "line") {
    p <- p + geom_path(position = position_dodge(dodge.width),
                       linewidth = line.thickness)
  }

  # Plot intervals if requested
  if (interval == TRUE & interval.geom[1] == "errorbar") {
    p <- p + geom_errorbar(aes(ymin = !! sym("ymin"), ymax = !! sym("ymax")),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           width = errorbar.width,
                           linewidth = line.thickness)
  } else if (interval == TRUE & interval.geom[1] %in% c("line", "linerange")) {
    p <- p + geom_linerange(aes(ymin = !! sym("ymin"), ymax = !! sym("ymax")),
                            alpha = 0.8, show.legend = FALSE,
                            position = position_dodge(dodge.width),
                            linewidth = line.thickness)
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~", as_string(mod2)))
    p <- p + facets
  }

  # For factor vars, plotting the observed points and coloring them by factor
  # looks great. Requires creating a separate layer to plot this other dataset.
  # If the moderator is numeric, will create a gradient color scheme.
  if (plot.points == TRUE) {

    if (!numeric_modx & point.shape) {
      shape_arg <- modx
    } else {
      shape_arg <- NULL
    }
    constants <- list(alpha = point.alpha)
    if (is.null(weights)) {
      # Only use constant size if weights are not used
      constants$size <- point.size
    }
    # Need to use layer function to programmatically define constant aesthetics
    p <- p + layer(geom = "point", data = d, stat = "identity",
                   inherit.aes = TRUE, show.legend = FALSE,
                   mapping = aes(x = !! pred, y = !! resp, size = !! weights,
                                 group = !! grp, colour = !! modx,
                                 shape = !! shape_arg),
                   position = if (!is.null(modx)) {
                      position_jitterdodge(dodge.width = dodge.width,
                                           jitter.width = jitter[1],
                                           jitter.height = jitter[2])
                     } else {
                       position_jitter(width = jitter[1], height = jitter[2])
                     },
                   params = constants) +
      scale_size(range = c(1 * point.size, 5 * point.size), guide = "none")

  }

  # Position of legend depends on facetting
  if (is.null(mod2)) {
    p <- p + theme_nice(legend.pos = "right")
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_nice(legend.pos = "bottom")
  }
  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Need different color scales depending on whether the moderator is numeric
  if (!numeric_modx) {
    p <- p + scale_colour_manual(name = legend.main, values = colors,
                                 breaks = names(colors),
                                 aesthetics = c("colour", "fill"))
  } else {
    limits <- quant(d[[modx]], probs = c(.1, .9))
    if (min2(modx.values) < limits[1]) {limits[1] <- min2(modx.values)}
    if (max2(modx.values) > limits[2]) {limits[2] <- max2(modx.values)}
    p <- p + scale_colour_gradientn(name = legend.main,
                                    breaks = modx.values,
                                    labels = modx.labels,
                                    colors = colors,
                                    limits = limits,
                                    oob = squish,
                                    aesthetics = c("colour", "fill"),
                                    guide = "legend")
  }
  p <- p + scale_shape(name = legend.main) +
    scale_x_discrete(limits = pred.values, labels = pred.labels)

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

  # Return the plot
  return(p)

}

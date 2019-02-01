
plot_predictions <- function(predictions, pred = NULL, modx = NULL, mod2 = NULL,
  resp = NULL, data = NULL, geom = c("point", "line", "bar", "boxplot"),
  plot.points = FALSE, interval = FALSE,
  pred.values = NULL, modx.values = NULL, mod2.values = NULL,
  linearity.check = FALSE,
  facet.modx = FALSE, x.label = NULL, y.label = NULL, pred.labels = NULL,
  modx.labels = NULL, mod2.labels = NULL, main.title = NULL, legend.main = NULL,
  colors = NULL, line.thickness = 1.1, vary.lty = NULL, jitter = 0,
  weights = NULL, rug = FALSE, rug.sides = "b", force.cat = FALSE,
  point.shape = FALSE, geom.alpha = NULL, dodge.width = NULL,
  errorbar.width = NULL, interval.geom = c("errorbar", "linerange"),
  pred.point.size = 3.5, point.size = 1, ...) {

  # Capture user-specified arguments
  # I'm more interested in the names than the actual content
  args <- as.list(match.call())
  args <- args[-1]

  if (class(predictions) == "predictions") {

    pred_obj <- predictions
    predictions <- pred_obj$predicted
    args$predictions <- predictions
    data <- pred_obj$original
    args$data <- data

    atts <- attributes(pred_obj)
    atts <- atts[names(atts) %nin% c("class","names")]

    # Renaming args that need to be different in this function
    # It ain't elegant and I should probability fix the ones feeding these
    # incorrect names
    if ("modxvals2" %in% names(atts)) {
      atts$modx.values <- atts$modxvals2
      atts <- atts[names(atts) %nin% "modxvals2"]
    }
    if ("mod2vals2" %in% names(atts)) {
      atts$mod2.values <- atts$mod2vals2
      atts <- atts[names(atts) %nin% "mod2vals2"]
    }
    if ("weights" %in% names(atts)) {
      atts$wts <- atts$weights
      atts <- atts[names(atts) %nin% "weights"]
    }

    for (n in names(atts)) {
      # This conditional prevents overwriting of user-specified args
      if (n %nin% names(args)) {
        assign(n, atts[[n]])
      }
    }

  }

  # Renaming these objects for compatibility with the plotting functions
  modxvals2 <- modx.values
  mod2vals2 <- mod2.values

  if (is.factor(predictions[[pred]]) | is.character(predictions[[pred]]) |
      force.cat == TRUE) {

    if (is.null(vary.lty)) {vary.lty <- FALSE}

    the_args <- formals("plot_cat")
    for (n in names(the_args)) {

      if (exists(n)) {
        the_args[[n]] <- get(n)
      }

    }

    the_args <- as.list(the_args)
    do.call("plot_cat", the_args)

  } else {

    if (is.null(vary.lty)) {vary.lty <- TRUE}

    the_args <- formals("plot_mod_continuous")
      for (n in names(the_args)) {

        if (exists(n)) {
          the_args[[n]] <- get(n)
        }

      }

      the_args <- as.list(the_args)
      do.call("plot_mod_continuous", the_args)

  }

}


plot_mod_continuous <- function(predictions, pred, modx, resp, mod2 = NULL,
  data = NULL, plot.points = FALSE, interval = FALSE, linearity.check = FALSE,
  x.label = NULL, y.label = NULL, pred.labels = NULL, modx.labels = NULL,
  mod2.labels = NULL, main.title = NULL, legend.main = NULL, colors = NULL,
  line.thickness = 1.1, vary.lty = TRUE, jitter = 0, modxvals2 = NULL,
  mod2vals2 = NULL, wts = NULL, rug = FALSE, rug.sides = "b",
  point.shape = FALSE, point.size = 2, facet.modx = FALSE) {

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
      colors <- "Blues"
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

  if (is.null(mod2)) {
    # colors <- rev(colors)
    low_color <- first(colors)
    high_color <- last(colors)
  } else {
    ltypes <- rev(ltypes)
    low_color <- last(colors)
    high_color <- first(colors)
  }

  if (gradient == FALSE) {
    names(colors) <- modx.labels
  }
  names(ltypes) <- modx.labels

  # Deal with non-syntactic names
  if (make.names(pred) !=  pred) {
    pred_g <- paste0("`", pred, "`")
  } else {
    pred_g <- pred
  }
  if (make.names(modx) !=  modx) {
    modx_g <- paste0("`", modx, "`")
  } else {
    modx_g <- modx
  }
  if (!is.null(mod2) && make.names(mod2) !=  mod2) {
    mod2_g <- paste0("`", mod2, "`")
  } else if (!is.null(mod2)) {
    mod2_g <- mod2
  }
  if (make.names(resp) !=  resp) {
    resp_g <- paste0("`", resp, "`")
  } else {
    resp_g <- resp
  }

  # Defining linetype here
  if (vary.lty == TRUE) {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, colour = modx_g,
                               group = "modx_group", linetype = "modx_group"))
  } else {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, colour = modx_g,
                               group = "modx_group"))
  }

  p <- p + geom_path(size = line.thickness, show.legend = !facet.modx)

  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes_string(x = pred_g,
                                               ymin = "ymin", ymax = "ymax",
                                               fill = modx_g,
                                               group = "modx_group",
                                               colour = modx_g, linetype = NA),
                         alpha = 1/5, show.legend = FALSE,
                         inherit.aes = TRUE)

    # p <- p + scale_fill_manual(name = legend.main, values = colors,
    #                            breaks = names(colors))
  }

  # If third mod, facet by third mod
  facet_form <- "~"
  modgroup <- NULL
  if (!is.null(mod2) || linearity.check == TRUE || facet.modx == TRUE) {
    do_facets <- TRUE
    # p <- p + facets
  } else {do_facets <- FALSE}

  if (linearity.check == TRUE | facet.modx == TRUE) {
    facet_form <- paste(facet_form, "modx_group")
    modgroup <- "modx_group"
  }

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
                        aes_string(x = pred_g, y = resp_g,
                                   group = "modx_group"),
                        method = "loess", size = 1,
                        show.legend = FALSE, inherit.aes = FALSE,
                        se = FALSE, span = 2, geom = "line",
                        alpha = 0.6, color = "red")
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    no_wts <- all(wts == 1)
    # const <- const * ((1 * !all(wts == 1)) + point.size)
    wts <- (const * point.size) * wts
    # Append weights to data
    d[["the_weights"]] <- wts

    if (!is.numeric(d[[modx]])) {
      # Create shape aesthetic argument
      shape_arg <- if (point.shape == TRUE) {modx_g} else {NULL}
      shape_guide <- if (point.shape == TRUE) {TRUE} else {FALSE}

      if (no_wts) {
        p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                                 colour = modx_g,
                                                 shape = shape_arg),
                            position = position_jitter(width = jitter[1],
                                                       height = jitter[2]),
                            inherit.aes = TRUE,
                            show.legend = shape_guide,
                            size = point.size) +
          scale_shape_discrete(name = legend.main, breaks = names(colors),
                               na.value = "blank")
      } else {
        p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                                 colour = modx_g,
                                                 size = "the_weights"
,                                                 shape = shape_arg),
                            position = position_jitter(width = jitter[1],
                                                       height = jitter[2]),
                            inherit.aes = TRUE,
                            show.legend = shape_guide) +
          scale_shape_discrete(name = legend.main, breaks = names(colors),
                               na.value = "blank") +
          # guides(shape = guide_legend(override.aes = list(size = point.size))) +
          scale_size_identity(guide = "none")
      }

    } else {
      p <- p + geom_point(data = d,
                          aes_string(x = pred_g, y = resp_g,
                                     size = "the_weights"),
                          inherit.aes = TRUE,
                          position = position_jitter(width = jitter[1],
                                                     height = jitter[2]),
                          show.legend = FALSE) +
        # scale_alpha_continuous(range = alpha_arg, guide = "none") +
        # Add size aesthetic to avoid giant points
        scale_size_identity(
          guide = "none"
        )
    }


  }

  # Rug plot for marginal distributions
  if (rug == TRUE) {
    p <- p + geom_rug(data = d,
                      aes_string(linetype = NULL),
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

  # Avoiding unnecessary import of scales --- this is scales::squish
  squish <- function(x, range = c(0, 1), only.finite = TRUE) {
    force(range)
    finite <- if (only.finite)
        is.finite(x)
    else TRUE
    x[finite & x < range[1]] <- range[1]
    x[finite & x > range[2]] <- range[2]
    x
  }

  # Get scale colors, provide better legend title
  if (!is.numeric(d[[modx]])) {
    p <- p + scale_colour_manual(name = legend.main, values = colors,
                                 breaks = names(colors),
                                 aesthetics = c("colour", "fill"))
  } else {
    limits <- quantile(d[[modx]], probs = c(.1, .9))
    if (min(modxvals2) < limits[1]) {limits[1] <- min(modxvals2)}
    if (max(modxvals2) > limits[2]) {limits[2] <- max(modxvals2)}
    if (length(colors) != 3) {
      p <- p + scale_colour_gradientn(name = legend.main,
                                   breaks = modxvals2,
                                   labels = modx.labels,
                                   # low = low_color,
                                   # high = high_color,
                                   colors = colors,
                                   limits = limits,
                                   oob = squish,
                                   aesthetics = c("colour", "fill"),
                                   guide = "legend")
    } else if (length(colors) == 3) {
      p <- p + scale_colour_gradient2(name = legend.main,
                                      breaks = modxvals2,
                                      labels = modx.labels,
                                      low = low_color,
                                      mid = colors[2],
                                      midpoint = modxvals2[2],
                                      high = high_color,
                                      # colors = colors,
                                      limits = limits,
                                      oob = squish,
                                      aesthetics = c("colour", "fill"),
                                      guide = "legend")
    }
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


plot_cat <- function(predictions, pred, modx = NULL, mod2 = NULL,
   data = NULL, geom = c("point", "line", "bar", "boxplot"), pred.values = NULL,
   modx.values = NULL, mod2.values = NULL, interval = TRUE, plot.points = FALSE,
   point.shape = FALSE, vary.lty = FALSE,  pred.labels = NULL,
   modx.labels = NULL, mod2.labels = NULL, x.label = NULL, y.label = NULL,
   main.title = NULL, legend.main = NULL, colors = "CUD Bright",
   wts = NULL, resp = NULL, jitter = 0.1, geom.alpha = NULL, dodge.width = NULL,
   errorbar.width = NULL, interval.geom = c("errorbar", "linerange"),
   line.thickness = 1.1, point.size = 1, pred.point.size = 3.5) {

  pm <- predictions
  d <- data

  geom <- geom[1]
  if (geom == "dot") {geom <- "point"}

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

  # Deal with non-syntactic names
  if (make.names(pred) !=  pred) {
    pred_g <- paste0("`", pred, "`")
  } else {
    pred_g <- pred
  }
  if (!is.null(modx) && make.names(modx) !=  modx) {
    modx_g <- paste0("`", modx, "`")
  } else if (!is.null(modx)) {
    modx_g <- modx
  }
  if (!is.null(mod2) && make.names(mod2) !=  mod2) {
    mod2_g <- paste0("`", mod2, "`")
  } else if (!is.null(mod2)) {
    mod2_g <- mod2
  }
  if (make.names(resp) !=  resp) {
    resp_g <- paste0("`", resp, "`")
  } else {
    resp_g <- resp
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

  if (!is.null(modx)) {
    gradient <- is.numeric(d[[modx]]) & !vary.lty
  } else {gradient <- FALSE}

  # Checking if user provided the colors his/herself
  colors <- suppressWarnings(get_colors(colors, length(modx.labels),
                                        gradient = gradient))

  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash",
             "12223242", "F282", "F4448444", "224282F2", "F1")
  ltypes <- types[seq_along(modx.labels)]

  names(ltypes) <- modx.labels
  names(colors) <- modx.labels

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

  if (is.null(dodge.width)) {
    dodge.width <- if (geom %in% c("bar", "point", "boxplot")) {0.9} else {0}
  }
  if (is.null(errorbar.width)) {
    errorbar.width <- if (geom == "point") {
      0.9
    } else if (geom == "bar") {
      0.75
    } else {0.5}
  }

  if (!is.null(modx)) {
    shape_arg <- if (point.shape == TRUE) {modx_g} else {NULL}
    lty_arg <- if (vary.lty == TRUE) {modx_g} else {NULL}

    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, group = modx_g,
                                 colour = modx_g, fill = modx_g,
                                 shape = shape_arg, linetype = lty_arg))
  } else {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, group = 1))
  }

  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level)
  } else if (geom == "boxplot") {
    if (!is.null(modx)) {
      p <- ggplot(d, aes_string(x = pred_g, y = resp_g,
                                colour = modx_g)) +
        geom_boxplot(position = position_dodge(dodge.width))
    } else {
      p <- ggplot(d, aes_string(x = pred_g, y = resp_g)) +
        geom_boxplot(position = position_dodge(dodge.width))
    }
  } else if (geom %in% c("point", "line")) {
    p <- p + geom_point(size = pred.point.size,
                        position = position_dodge(dodge.width))
  }

  if (geom == "line") {
    p <- p + geom_path(position = position_dodge(dodge.width),
                       size = line.thickness)
  }

  # Plot intervals if requested
  if (interval == TRUE && geom != "boxplot" && interval.geom == "errorbar") {
    p <- p + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           width = errorbar.width,
                           size = line.thickness)
  } else if (interval == TRUE && geom != "boxplot" && interval.geom %in%
                                                    c("line", "linerange")) {
    p <- p + geom_linerange(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 0.8, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           size = line.thickness)
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~", mod2_g))
    p <- p + facets
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts) / sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    const <- const * (1 * all(wts == 1) * point.size)
    wts <- const * wts
    # Append weights to data
    d[,"the_weights"] <- wts

    if (!is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                               colour = modx_g,
                                               size = "the_weights",
                                               shape = shape_arg),
                          position =
                            position_jitterdodge(dodge.width = dodge.width,
                                                 jitter.width = jitter[1],
                                                 jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    } else if (is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                               size = "the_weights",
                                               shape = pred_g),
                          position =
                            position_jitterdodge(dodge.width = dodge.width,
                                                 jitter.width = jitter[1],
                                                 jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    }

    # Add size aesthetic to avoid giant points
    p <- p + scale_size_identity()

  }

  # Using theme_apa for theming...but using legend title and side positioning
  if (is.null(mod2)) {
    p <- p + theme_nice(legend.pos = "right")
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_nice(legend.pos = "bottom")
  }
  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Get scale colors, provide better legend title
  p <- p + scale_colour_manual(name = legend.main,
                               values = colors,
                               breaks = names(colors))
  p <- p + scale_fill_manual(name = legend.main,
                             values = colors,
                             breaks = names(colors))
  p <- p + scale_shape(name = legend.main)

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

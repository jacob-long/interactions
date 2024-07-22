#' Perform a simple margins analysis.
#'
#' \code{sim_margins} conducts a simple margins analysis for the purposes of
#' understanding two- and three-way interaction effects in linear regression.
#'
#' @param ... ignored.
#'
#' @details This allows the user to perform a simple margins analysis for the
#'   purpose of probing interaction effects in a linear regression. Two- and
#'   three-way interactions are supported, though one should be warned that
#'   three-way interactions are not easy to interpret in this way.
#'
#'   The function is tested with `lm`, `glm`, `svyglm`, and `merMod` inputs.
#'   Others may work as well, but are not tested. In all but the linear model
#'   case, be aware that not all the assumptions applied to simple slopes
#'   analysis apply.
#'
#' @return
#'
#'  A list object with the following components:
#'
#'  \item{slopes}{A table of coefficients for the focal predictor at each
#'  value of the moderator}
#'  \item{ints}{A table of coefficients for the intercept at each value of the
#'    moderator}
#'  \item{modx.values}{The values of the moderator used in the analysis}
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @inheritParams sim_slopes
#' @inheritParams margins::margins
#'
#' @seealso [margins::margins()]
#'
#' @family interaction tools
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral Research}, \emph{40}(3), 373-400.
#'  \doi{10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple regression/correlation analyses for the behavioral sciences} (3rd
#' ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' Hanmer, M. J., & Kalkan, K. O. (2013). Behind the curve: Clarifying the best
#' approach to calculating predicted probabilities and marginal effects from
#' limited dependent variable models. *American Journal of Political Science*,
#' *57*, 263–277. \doi{10.1111/j.1540-5907.2012.00602.x}
#'
#'
#' @importFrom stats coef coefficients lm predict sd update getCall vcov relevel
#' @importFrom stats family aggregate formula
#' @import jtools
#' @export
#'

sim_margins <- function(model, pred, modx, mod2 = NULL, modx.values = NULL,
                       mod2.values = NULL, data = NULL, cond.int = FALSE,
                       vce = c("delta", "simulation", "bootstrap", "none"),
                       iterations = 1000,
                       digits = getOption("jtools-digits", default = 2),
                       pvals = TRUE, confint = FALSE, ci.width = .95,
                       cluster = NULL, modx.labels = NULL, mod2.labels = NULL,
                       ...) {

  if (!requireNamespace("margins")) {
    stop_wrap("You must have the margins package installed to use this
              function.")
  }

  # Evaluate the modx, mod2, pred args
  pred <- quo_name(enexpr(pred))
  modx <- quo_name(enexpr(modx))
  if (modx == "NULL") {modx <- NULL}
  mod2 <- quo_name(enexpr(mod2))
  if (mod2 == "NULL") {mod2 <- NULL}

  # Warn user if interaction term is absent
  if (!check_interactions(as.formula(formula(model)), c(pred, modx, mod2))) {
    warn_wrap(c(pred, modx, mod2), " are not included in an interaction with
              one another in the model.")
  }

  # Create object to return
  ss <- list()

  ss <- structure(ss, digits = digits)

  d <- get_data(model)
  if (is_survey <- inherits(model, "svyglm")) {
    design <- model$survey.design
  } else {design <- NULL}
  # Which variables are factors?
  facvars <- names(d)[!unlist(lapply(d, is.numeric))]
  fvars <- names(d)

  # Check for factor predictor
  if (is.factor(d[[pred]])) {
    # I could assume the factor is properly ordered, but that's too risky
    stop(wrap_str("Focal predictor (\"pred\") cannot be a factor. Either
          use it as modx or convert it to a numeric dummy variable."))
  }

  wname <- get_weights(model, d)$weights_name
  wts <- get_weights(model, d)$weights
  offname <- get_offset_name(model)
  # Need the raw variable name from the LHS
  resp <- all.vars(as.formula(paste("~", (get_response_name(model)))))

  # Saving key arguments as attributes of return object
  ss <- structure(ss, resp = resp, modx = modx, mod2 = mod2, pred = pred,
                  cond.int = cond.int)

### Getting moderator values ##################################################

  modxvals2 <- mod_vals(d, modx, modx.values, is_survey, wts, design,
                        modx.labels = modx.labels,
                        any.mod2 = !is.null(mod2), sims = TRUE)

  # Now specify def or not (for labeling w/ print method)
  if (is.character(modx.values) | is.null(modx.values)) {

    ss <- structure(ss, def = TRUE)

  } else {

    ss <- structure(ss, def = FALSE)

  }

  # Don't want def = TRUE for factors even though they are character
  if (!is.numeric(d[[modx]])) {ss <- structure(ss, def = FALSE)}

  if (!is.null(mod2)) {

    if (is.numeric(d[[mod2]])) {
      mod2vals2 <- mod_vals(d, mod2, mod2.values, is_survey, wts, design,
                            modx.labels = mod2.labels,
                            any.mod2 = !is.null(mod2),
                            sims = TRUE)
    } else {

      if (is.null(mod2.values)) {
        mod2vals2 <- levels(d[[mod2]])
      } else {
        if (all(mod2.values %in% unique(d[[mod2]]))) {
          mod2vals2 <- mod2.values
        } else {
          warn_wrap("mod2.values argument must include only levels of the
                    factor. Using all factor levels instead.", call. = FALSE)
          mod2vals2 <- unique(d[[mod2]])
        }
      }

    }

    # Now specify def or not
    if (is.character(mod2.values) | is.null(mod2.values)) {

      ss <- structure(ss, def2 = TRUE)

    } else {

      ss <- structure(ss, def2 = FALSE)

    }

    # Don't want def = TRUE for factors even though they are character
    if (!is.numeric(d[[mod2]])) {ss <- structure(ss, def2 = FALSE)}

  } else {
    mod2vals2 <- NULL
    modxvals2 <- rev(modxvals2)
  }

#### Call margins #############################################################

  # Create list to provide to `at` argument
  at_list <- list()
  at_list[[modx]] <- modxvals2
  if (!is.null(mod2)) {
    at_list[[mod2]] <- mod2vals2
  }

  design <- if (inherits(model, "svyglm")) model$survey.design else NULL

  # Get the margins
  suppressWarnings({ # can't have confusing warnings from margins
    margs <- margins::margins(model, data = d, at = at_list, vce = vce,
                     # don't need modx, but it works around margins issue #112
                     variables = c(pred, modx),
                     iterations = iterations, design = design
             )
  })
  # Get the summary data frame, drop the modx rows, drop the "factor" column
  slopes <- subset(summary(margs, level = ci.width), factor == pred) %not%
    "factor"
  # determine if we're using t or z values
  t_or_z <-
    if (family(model)$family == "gaussian" & family(model)$link == "identity") {
      "t val."
    } else  {
      "z val."
    }
  names(slopes) %just% c("AME", "SE", "z", "lower", "upper") <-
    c("Est.", "S.E.", t_or_z, unlist(make_ci_labs(ci.width)))

  # Get the conditional intercepts
  # Using aggregate to take the means at each combination of modx and mod2
  ## Have to jump through some hoops to weight the means
  num_per <- nrow(margs) /
    nrow(expand.grid(modxvals2, if (!is.null(mod2vals2)) mod2vals2 else NA))
  ## num_per is how many rows there are per set of modx/mod2 in the margs obj
  ints <- aggregate(margs[c("fitted")], margs[c(modx, mod2)],
                    mean_or_base, weights = margs[["_weights"]][1:num_per])
  names(ints) %just% "fitted" <- "intercept"
  # Add the intercepts directly to the slopes data.frame since I don't have
  # variance estimates for them anyway
  slopes <- merge(slopes, ints)
  if (!is.null(mod2)) {
    # While we're here, let's split the slopes data.frame into pieces by mod2
    slopes <- split(slopes, slopes[mod2])
    names(slopes) <- paste(mod2, "=", names(mod2vals2))
  }

  ss <- structure(ss, modx.values = modxvals2, vce = vce,
                  cond.int = cond.int, confint = confint,
                  ci.width = ci.width, t_or_z = t_or_z,
                  nobs = nobs(model))

  ss$slopes <- slopes
  ss$ints <- ints

  if (!is.null(mod2)) {ss <- structure(ss, mod2.values = mod2vals2)}

  class(ss) <- "sim_margins"

  return(ss)

}


#### PRINT METHOD ############################################################

#' @export
#' @importFrom cli cat_rule rule

print.sim_margins <- function(x, ...) {

  # This is to make refactoring easier after switch to attributes
  ss <- x
  x <- attributes(x)

  # This helps deal with the fact sometimes mod2vals has no length, so we want
  # to loop just once
  if (!is.null(x$mod2)) {
    length <- length(x$mod2.values)
  } else {
    length <- 1
  }

  # Loop through each value of second moderator...if none, just one loop
  for (j in 1:length) {

    m <- NULL

    # If we're using second moderator, need to make things make sense
    # to inner loop
    if (!is.null(x$mod2)) {

      m <- ss$slopes[[j]]

      if (inherits(x$mod2.values, "character")) {
        m[x$mod2] <- num_print(m[x$mod2], x$digits)
      }

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator) ",
                                 "= ", m[j, x$mod2]), line = "bar8"),
            "\n\n")
      } else {
        # If the user went with default +/- SD or used a factor variable,
        # we use the labels
        label <- names(x$mod2.values)[
          which(num_print(x$mod2.values, x$digits) == as.vector(m[j, x$mod2]))
        ]
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator)", " = ",
                 m[j, x$mod2], " (", label, ")"), line = "bar8"), "\n\n")
      }

      m <- m %not% x$mod2

    } else {
      m <- ss$slopes
    }

    if (x$confint == FALSE) {
      m <- m %not% unlist(make_ci_labs(x$ci.width))
    }
    if (x$cond.int == FALSE) {
      m <- m %not% "intercept"
    }

    # Clearly label simple slopes
    cli::cat_line(
      cli::style_bold(cli::style_underline("SIMPLE MARGINS")),
      "\n"
    )

    for (i in seq_along(x$modx.values)) {

      if (inherits(x$modx.values, "character")) {
        m[x$modx] <- num_print(m[x$modx], digits = x$digits)
      }

      slopes <- as.data.frame(lapply(m[i,2:ncol(m)], as.numeric),
                  check.names = FALSE)

      # Handle automatic labels
      if (x$def == TRUE) {
        label <- names(x$modx.values)[
          which(num_print(x$modx.values, x$digits) == as.vector(m[i, x$modx]))
        ]
        modx_label <- paste0(m[i, x$modx], " (", label, ")")
      } else {
        modx_label <- paste0(m[i, x$modx])
      }

      cli::cat_line(cli::style_italic(paste0("Average marginal effect of ", x$pred, " when ",
                        x$modx, " = ", modx_label, ": \n")))
      print(md_table(slopes, digits = x$digits, format = "pandoc",
                     row.names = FALSE, sig.digits = FALSE))

      cat("\n")

    }
  } # end mod2 loop

}

#### alternate output formats ################################################

#' @title Tidiers for [sim_margins()] objects.
#' @description You can use [broom::tidy()] and [broom::glance()] for "tidy"
#'  methods of storing `sim_margins` output.
#' @param x The `sim_margins` object
#' @param conf.level The width of confidence intervals. Default is .95 (95%).
#' @param ... Ignored.
#' @rdname sim_margins_tidiers
#' @export

tidy.sim_margins <- function(x, conf.level = .95, ...) {

  cols <- c("estimate", "std.error", "statistic", "p.value", "modx",
            "modx.value", "mod2", "mod2.value", "intercept")
  # Figure out how many rows the data frame will be
  num_coefs <- ifelse(is.data.frame(x$slopes),
                      no = length(x$slopes) * nrow(x$slopes[[1]]),
                      yes = nrow(x$slopes))
  # Create NA-filled data frame
  base <- as.data.frame(matrix(rep(NA, times = num_coefs * length(cols)),
                               ncol = length(cols)))
  # Name the columns
  names(base) <- cols

  # Get the attributes from the sim_slopes object
  atts <- attributes(x)

  # Is there a second moderator?
  any_mod2 <- !is.null(atts$mod2)
  if (any_mod2 == FALSE) {
    all_slopes <- x$slopes
  } else {
    all_slopes <- do.call("rbind", x$slopes)
  }

  # Include the moderator name (probably not best to include this redundant
  # info)
  base$modx <- atts$modx

  # Move the table of values to the data frame
  base$modx.value <- all_slopes[,1]
  base$estimate <- all_slopes[,"Est."]
  base$std.error <- all_slopes[,"S.E."]
  base$p.value <- all_slopes[,"p"]
  base$statistic <- all_slopes[, grep("val.", colnames(all_slopes), value = T)]
  base$intercept <- all_slopes[,"intercept"]

  # Handle CIs
  ## These are the requested CI labels
  want_labs <- unlist(make_ci_labs(conf.level))
  ## Check if those are already calculated
  if (all(want_labs %in% colnames(all_slopes))) {
    base$conf.low <- all_slopes[,make_ci_labs(conf.level)[[1]]]
    base$conf.high <- all_slopes[,make_ci_labs(conf.level)[[2]]]
  } else { # If not, calculate them
    alpha <- (1 - conf.level) / 2
    crit_t <- if (inherits(x$mods[[1]], "lm")) {
      abs(qt(alpha, df = df.residual(x$mods[[1]])))
    } else {
      abs(qnorm(alpha))
    }
    base$conf.low <- base$estimate - (crit_t * base$std.error)
    base$conf.high <- base$estimate + (crit_t * base$std.error)
  }

  # Create unique term labels for each value of the moderator
  base$term <- paste(base$modx, "=",
                     if (is.character(base$modx.value)) {
                       base$modx.value
                     } else {
                       num_print(base$modx.value, attr(x, "digits"))
                     }
  )

  # Do the same for moderator 2 if any
  if (any_mod2 == TRUE) {
    base$mod2 <- atts$mod2
    base$mod2.value <- unlist(lapply(atts$mod2.values, function(y) {
      rep(y, nrow(x$slopes[[1]]))
    }))

    base$mod2.term <- paste(base$mod2, "=",
                            if (is.character(base$mod2.value)) {
                              base$mod2.value
                            } else {
                              num_print(base$mod2.value, attr(x, "digits"))
                            }
    )
  }

  base <- tibble::as_tibble(base)
  attr(base, "pred") <- atts$pred

  return(base)

}

#' @rdname sim_margins_tidiers
#' @export

glance.sim_margins <- function(x, ...) {
  tibble::as_tibble(data.frame(N = nobs(x)))
}

#' @importFrom stats nobs
#' @export

nobs.sim_margins <- function(object, ...) {
  attr(object, "nobs")
}


#' @title Create tabular output for simple margins analysis
#'
#' @description This function converts a `sim_margins` object into a
#' `huxtable` object, making it suitable for use in external documents.
#' @param x A `sim_margins` object.
#' @inheritParams as_huxtable.sim_slopes
#'
#' @details
#'
#' For more on what you can do with a `huxtable`, see \pkg{huxtable}.
#'
#' @rdname as_huxtable.sim_margins
#' @rawNamespace
#' if (getRversion() >= "3.6.0") {
#'   S3method(huxtable::as_huxtable, sim_margins)
#' } else {
#'   export(as_huxtable.sim_margins)
#' }

as_huxtable.sim_margins <-  function(x, format = "{estimate} ({std.error})",
  sig.levels = c(`***` = .001, `**` = .01, `*` = .05, `#` = .1),
  digits = getOption("jtools-digits", 2), conf.level = .95,
  intercept = attr(x, "cond.int"), int.format = format, ...) {

  df <- tidy.sim_margins(x, conf.level = conf.level)
  make_table(df = df, format = format, sig.levels = sig.levels, digits = digits,
             label = "Average marginal effect of",
             intercept = if (attr(x, "cond.int")) df$intercept else NULL)

}

#' @export
#' @title Plot coefficients from simple slopes analysis
#' @description This creates a coefficient plot to visually summarize the
#' results of simple slopes analysis.
#' @param x A [sim_margins()] object.
#' @param ... arguments passed to [jtools::plot_coefs()]

plot.sim_margins <- function(x, ...) {
  # Get the plot and add better x-axis label
  p <- plot_coefs(x, ...) + ggplot2::xlab(paste("Average marginal effect of",
                                                attr(x, "pred")))

  # If there's a second moderator, format as appropriate
  if (!is.null(attr(x, "mod2"))) {
    p <- p + ggplot2::facet_wrap(mod2.term ~ ., ncol = 1, scales = "free_y",
                                 strip.position = "top")
  }

  p
}

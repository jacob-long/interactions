#' Probe interaction effects via simple slopes and plotting
#'
#' \code{probe_interaction} is a convenience function that allows users to call
#' both \code{\link{sim_slopes}} and \code{\link{interact_plot}} with a single
#' call.
#'
#' @usage probe_interaction(model, pred, modx, mod2 = NULL, ...)
#'
#' @inheritParams interact_plot
#'
#' @param ... Other arguments accepted by \code{\link{sim_slopes}} and
#'  \code{\link{interact_plot}}
#'
#' @details
#'
#' This function simply merges the nearly-equivalent arguments needed to call
#' both \code{\link{sim_slopes}} and \code{\link{interact_plot}} without the
#' need for re-typing their common arguments. Note that each function is called
#' separately and they re-fit a separate model for each level of each
#' moderator; therefore, the runtime may be considerably longer than the
#' original model fit. For larger models, this is worth keeping in mind.
#'
#' Sometimes, you may want different parameters when doing simple slopes
#' analysis compared to when plotting interaction effects. For instance, it is
#' often easier to interpret the regression output when variables are
#' standardized; but plots are often easier to understand when the variables
#' are in their original units of measure.
#'
#' \code{probe_interaction} does not
#' support providing different arguments to each function. If that is needed,
#' use \code{sim_slopes} and \code{interact_plot} directly.
#'
#' @return
#'
#' \item{simslopes}{The \code{sim_slopes} object created.}
#' \item{interactplot}{The \code{ggplot} object created by
#' \code{interact_plot}.}
#'
#' @family interaction tools
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder * Illiteracy,
#'   data=as.data.frame(state.x77))
#' probe_interaction(model = fiti, pred = Murder, modx = Illiteracy,
#'                   modx.values = "plus-minus")
#' # 3-way interaction
#' fiti3 <- lm(Income ~ Frost * Murder * Illiteracy,
#'   data=as.data.frame(state.x77))
#' probe_interaction(model = fiti3, pred = Murder, modx = Illiteracy,
#'                   mod2 = Frost, mod2.values = "plus-minus")
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals + sch.wide, design = dstrat)
#' probe_interaction(model = regmodel, pred = ell, modx = meals,
#'                   modx.values = "plus-minus", cond.int = TRUE)
#'
#' # 3-way with survey and factor input
#' regmodel3 <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
#' probe_interaction(model = regmodel3, pred = ell, modx = meals,
#'                   mod2 = sch.wide)
#' # Can try different configurations of 1st vs 2nd mod
#' probe_interaction(model = regmodel3, pred = ell, modx = sch.wide,
#'                   mod2 = meals)
#' }
#'
#' @export


probe_interaction <- function(model, pred, modx, mod2 = NULL, ...) {

  # Create list of acceptable arguments to both functions
  ssnames <- names(formals(sim_slopes))
  ipnames <- names(formals(interact_plot))

  # Capture the arguments
  dots <- list(...)

  # Capture explicit args
  args <- match.call()

  # Add the actual arguments
  dots <- c(dots, model = args$model, modx = args$modx, pred = args$pred, 
            mod2 = args$mod2)

  # Create list of arguments accepted by sim_slopes
  ssargs <- dots[names(dots) %in% ssnames]
  # Create list of arguments accepted by interact_plot
  ipargs <- dots[names(dots) %in% ipnames]

  # the "alpha" argument in johnson_neyman is "jnalpha" for sim_slopes
  if ("alpha" %in% names(dots)) {
    ssargs[["jnalpha"]] <- dots[["alpha"]]
  }

  # Call sim_slopes
  ss <- do.call("sim_slopes", ssargs)
  # Call interact_plot
  ip <- do.call("interact_plot", ipargs)

  # Save both to output object
  out <- list()
  out$simslopes <- ss
  out$interactplot <- ip

  # Set class for print method
  class(out) <- "probe_interaction"

  return(out)

}

#' @export

print.probe_interaction <- function(x, ...) {

  print(x$simslopes)
  print(x$interactplot)

}

#### Non-exported functions ###################################################

## Utility function for getting values of moderator values for interaction
## functions

mod_vals <- function(d, modx, modx.values, survey, weights,
                     design = design, modx.labels = NULL,
                     any.mod2 = FALSE, is.mod2 = FALSE,
                     sims = FALSE, facet.modx = FALSE, force.cat = FALSE,
                     add.varname = TRUE) {

  # Get moderator mean
  if (survey == FALSE & is.numeric(d[[modx]])) {
    weights <- if (is.null(weights)) {
      rep(1, nrow(d))
    } else if (is.character(weights)) {
      d[[weights]]
    } else weights
    modmean <- weighted.mean(d[[modx]], weights, na.rm = TRUE)
    modsd <- wtd.sd(d[[modx]], weights)

  } else if (survey == TRUE & is.numeric(d[[modx]])) {

    modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
    # Have to construct the formula this way since the syntax for svymean
    # differs from mean
    modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")),
                               design = design)

  }

  is_fac <- if (!is.numeric(d[[modx]]) | force.cat == TRUE) TRUE else FALSE

  # Testing whether modx.values refers to pre-defined arg or list of factor levels
  predefined_args <- c("mean-plus-minus", "plus-minus", "terciles")
  if (is.character(modx.values) & length(modx.values) == 1) {
    char1 <- if (modx.values %in% predefined_args) TRUE else FALSE
    if (is_fac == TRUE & char1 == TRUE) {
      stop_wrap(modx.values, " is not supported for a non-numeric moderator.")
    } else if (is_fac == FALSE & char1 == FALSE) {
      stop_wrap(modx.values, " is not a valid ",
                ifelse(is.mod2, yes = "mod2.values", no = "modx.values"),
                " argument for a numeric moderator.")
    }
  } else {char1 <- FALSE}

  user_specified <- length(modx.values) > 1

  # If using a preset, send to auto_mod_vals function
  if (is_fac == FALSE && (is.null(modx.values) | is.character(modx.values))) {

    modxvals2 <- auto_mod_vals(d, modx.values = modx.values, modx = modx,
                               modmean = modmean, modsd = modsd,
                               modx.labels = modx.labels,
                               mod2 = (is.mod2 | facet.modx),
                               sims = sims, add.varname = add.varname)

  }

  # For user-specified numbers or factors, go here
  if (is.null(modx.values) & is_fac == TRUE) {

    modxvals2 <- ulevels(d[[modx]])
    if (is.null(modx.labels)) {

      if ((is.mod2 | facet.modx) & add.varname == TRUE) {
        modx.labels <- paste(modx, "=", ulevels(d[[modx]]))
      } else {
        modx.labels <- ulevels(d[[modx]])
      }

    }
    names(modxvals2) <- modx.labels

  } else if (!is.null(modx.values) & char1 == FALSE) {
    # Use user-supplied values otherwise

    if (!is.null(modx.labels)) {
      # What I'm doing here is preserving the label order
      names(modx.values) <- modx.labels
      if (!is.mod2 & !is.factor(d[[modx]])) {
        modxvals2 <- rev(modx.values)
      } else {
        modxvals2 <- modx.values
      }
      modx.labels <- names(modxvals2)

    } else {

      names(modx.values) <- if ((is.mod2 | facet.modx) & add.varname == TRUE) {
        paste(modx, "=", modx.values)
      } else {
        modx.values
      }
      if (!is.mod2 & !is.factor(d[[modx]])) {
        modxvals2 <- rev(modx.values)
      } else {
        modxvals2 <- modx.values
      }
      modx.labels <- names(modxvals2)

    }

  }

  if (is.null(modx.labels)) {
    # Name the modx.labels object with modxvals2 names

    modx.labels <- if ((is.mod2 | facet.modx) & add.varname == TRUE) {
      paste(modx, "=", modxvals2)
    } else {
      names(modxvals2)
    }

  }

  # Hacky way to have a shorthand to drop NA
  range2 <- function(...) {
    range(..., na.rm = TRUE)
  }
  if (is_fac == FALSE & user_specified == FALSE) {
    # The proper order for interact_plot depends on presence of second moderator
    modxvals2 <- sort(modxvals2, decreasing = (!any.mod2 & !facet.modx))
    if (any(modxvals2 > range2(d[[modx]])[2])) {
      warn_wrap(paste(modxvals2[which(modxvals2 > range2(d[[modx]])[2])],
                      collapse = " and "), " is outside the observed range of ",
                modx)
    }
    if (any(modxvals2 < range2(d[[modx]])[1])) {
      warn_wrap(paste(modxvals2[which(modxvals2 < range2(d[[modx]])[1])],
                      collapse = " and "), " is outside the observed range of ",
                modx)
    }
  }

  return(modxvals2)

}

## Gets the preset values, e.g., mean plus/minus 1 SD

auto_mod_vals <-
  function(d, modx, modx.values, modmean, modsd, modx.labels = NULL,
           mod2 = FALSE, sims = FALSE, add.varname = TRUE) {

    # Default to +/- 1 SD unless modx is factor
    if ((is.null(modx.values) || modx.values == "mean-plus-minus") &
        length(unique(d[[modx]])) > 2) {

      modxvals2 <- c(modmean - modsd,
                     modmean,
                     modmean + modsd)
      if (mod2 == FALSE) {
        names(modxvals2) <- c("- 1 SD", "Mean", "+ 1 SD")
      } else {
        names(modxvals2) <- c(paste("Mean of", modx, "- 1 SD"),
                              paste("Mean of", modx),
                              paste("Mean of", modx, "+ 1 SD"))
      }

    } else if (!is.null(modx.values) && modx.values[1] == "plus-minus") { # No mean

      modxvals2 <- c(modmean - modsd, modmean + modsd)
      if (mod2 == FALSE) {
        names(modxvals2) <- c("- 1 SD", "+ 1 SD")
      } else {
        names(modxvals2) <- c(paste("Mean of", modx, "- 1 SD"),
                              paste("Mean of", modx, "+ 1 SD"))
      }

    } else if (!is.null(modx.values) && modx.values[1] == "terciles") {

      x_or_2 <- switch(as.character(mod2),
                       "TRUE" = "2",
                       "FALSE" = "x")
      group_name <- paste0("mod", x_or_2)
      d[[group_name]] <- cut2(d[[modx]], g = 3, levels.median = TRUE)
      modxvals2 <- as.numeric(levels(d[[group_name]]))
      msg_wrap("Medians of each tercile of ", modx, " are ",
               paste(modxvals2, collapse = ", "))

      if (mod2 == FALSE) {
        names(modxvals2) <- c("Lower tercile median", "Middle tercile median",
                              "Upper tercile median")
      } else {
        names(modxvals2) <- c(paste("Lower tercile of", modx),
                              paste("Middle tercile of", modx),
                              paste("Upper tercile of", modx))
      }

    } else if (is.null(modx.values) & length(unique(d[[modx]])) == 2) {

      modxvals2 <- as.numeric(levels(factor(d[[modx]])))
      if (!is.null(modx.labels)) {

        names(modxvals2) <- modx.labels

      } else {

        if (mod2 == TRUE & sims == FALSE & add.varname == TRUE) {
          names(modxvals2) <-
            sapply(modxvals2, FUN = function(x) {paste(modx, "=", round(x,3))})
        } else {
          names(modxvals2) <- modxvals2
        }

      }

    }

    if (!is.null(modx.labels)) {
      if (length(modx.labels) == length(modxvals2)) {
        names(modxvals2) <- modx.labels
      } else {
        warn_wrap("modx.labels or mod2.labels argument is not the same length
                  as the number of moderator values used. It will be ignored.")
      }
    }

    return(modxvals2)

  }


## Centering

center_ss <- function(d, weights, facvars = NULL, fvars, pred, resp, modx,
                      survey, design = NULL, mod2, wname, offname, centered,
                      at = NULL) {

  # Just need to pick a helper function based on survey vs no survey
  if (survey == TRUE) {

    out <- center_ss_survey(d, weights, facvars, fvars, pred, resp, modx,
                            survey, design, mod2, wname, offname, centered, at)

  } else {

    out <- center_ss_non_survey(d, weights, facvars, fvars, pred, resp, modx,
                                mod2, wname, offname, centered, at)

  }

  return(out)


}

## If not svydesign, centering is fairly straightforward

center_ss_non_survey <- function(d, weights, facvars = NULL, fvars, pred,
                                 resp, modx, mod2, wname, offname, centered, at) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (centered[1] != "all" && centered[1] != "none") {

    if (any(omitvars %in% centered)) {
      warn_wrap("Moderators, outcome variables, and weights/offsets
                cannot be centered.")
      centered <- centered[centered %nin% omitvars]
    }
    if (length(centered) > 0) {
      d <- gscale(vars = centered, data = d, center.only = TRUE,
                  weights = weights)
    }

    for (v in fv2) {

      if (is.factor(d[[v]]) &&
          length(unique(d[[v]])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }

    }

  } else if (centered[1] == "all") {

    # saving all vars except response
    vars <- names(d)[names(d) %nin% omitvars]

    d <- gscale(vars = vars, data = d, center.only = TRUE,
                weights = weights)

  } else if (centered == "none") {

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) && length(unique(d[[v]])) == 2) {

        facvars <- c(facvars, v)

      }
    }

  }

  if (!is.null(at)) {
    d <- set_at(at = at, d = d)
  }

  out <- list(d = d, facvars = facvars, fvars = fvars, design = NULL)

  return(out)

}

## Svydesigns get their own function to make control flow easier to follow

center_ss_survey <- function(d, weights, facvars = NULL, fvars, pred, resp,
                             modx, survey, design, mod2, wname, offname,
                             centered, at) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname, names(at))

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (centered[1] != "all" && centered[1] != "none") {

    if (any(omitvars %in% centered)) {
      warn_wrap("Moderators, outcome variables, and weights/offsets cannot be 
                 centered.")
      centered <- centered[centered %nin% omitvars]
    }
    design <- gscale(vars = centered, data = design, center.only = TRUE)
    d <- design$variables

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) &&
          length(unique(d[[v]])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }
    }

  } else if (centered == "none") {
    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) && length(unique(d[[v]])) == 2) {
        facvars <- c(facvars, v)
      }
    }

  } else if (centered == "all") {
    # Center all non-focal
    ndfvars <- fvars[fvars %nin% omitvars]

    if (length(ndfvars) > 0) {
      design <- gscale(vars = ndfvars, data = design, center.only = TRUE)
      d <- design$variables
    }
  }

  if (!is.null(at)) {
    d <- set_at(at = at, d = d)
  }

  out <- list(d = d, design = design, facvars = facvars, fvars = fvars)

  return(out)
}

#### Deal with at variables #################################################
set_at <- function(at, d) {
  for (v in names(at)) {
    if (v %nin% names(d)) stop_wrap("`at` variable ", v, " not found in data.")
    if (!is.numeric(d[[v]])) {
      warn_wrap("Inclusion of non-numeric variable ", v, " in `at` argument
                is not currently supported. As an alternative, treat the 
                variable as a factor and use the relevel() function to
                set this value as its reference level before fitting your
                model.")
    } else {
      d[[v]] <- d[[v]] - at[[v]]
    }
  }
  return(d)
}

#### Send deprecation warnings ##############################################

ss_dep_check <- function(fun_name, dots) {

  dep_names <- c("scale", "standardize")
  if (any(names(dots) %in% dep_names)) {
    warn_wrap(fun_name, " no longer supports variable scaling. You can use
              gscale to scale your data or scale_mod to scale your model.")
  }

}

#### Check for interactions ##################################################

any_interaction <- function(formula) {
  any(attr(terms(formula), "order") > 1)
}

get_interactions <- function(formula) {
  if (any_interaction(formula)) {
    ts <- terms(formula)
    labs <- paste("~", attr(ts, "term.labels"))
    forms <- lapply(labs, as.formula)
    forms <- forms[which(attr(ts, "order") > 1)]
    ints <- lapply(forms, all.vars)
    names(ints) <- attr(ts, "term.labels")[which(attr(ts, "order") > 1)]
    return(ints)
  } else {
    NULL
  }
}

check_interactions <- function(formula, vars) {
  vars <- vars[!is.null(vars)]
  if (any_interaction(formula)) {
    checks <- sapply(get_interactions(formula), function(x, vars) {
      if (all(vars %in% x)) TRUE else FALSE
    }, vars = vars)
    any(checks)
  } else {
    FALSE
  }
}

#### predict helpers ########################################################

values_checks <- function(pred.values = NULL, modx.values, mod2.values) {

  if (length(pred.values) == 1) {
    stop("pred.values must be at least a length of 2.", call. = FALSE)
    }
  if (length(modx.values) == 1 &&
      modx.values %nin% c("plus-minus", "terciles")) {
    stop("modx.values must be at least a length of 2.", call. = FALSE)
  }
  if (length(mod2.values) == 1 &&
      mod2.values %nin% c("plus-minus", "terciles")) {
    stop("mod2.values must be at least a length of 2.", call. = FALSE)
  }

}

#' @importFrom stats as.formula complete.cases df.residual model.frame pt
#' @importFrom stats residuals terms weighted.mean
prep_data <- function(model, d, pred, modx, mod2, pred.values = NULL,
                      modx.values, mod2.values, survey, pred.labels = NULL,
                      modx.labels, mod2.labels, wname, weights,
                      linearity.check, interval, set.offset, facvars, centered,
                      preds.per.level, force.cat = FALSE, facet.modx = FALSE,
                      partial.residuals = FALSE, outcome.scale, at, ...) {
  # offset?
  offname <- jtools::get_offset_name(model)
  off <- !is.null(offname)

  if (!is.numeric(d[[pred]])) {
    facpred <- TRUE
    if (is.character(d[[pred]])) {d[[pred]] <- factor(d[[pred]])}
  } else if (force.cat == FALSE) {
    facpred <- FALSE
  } else {
    facpred <- TRUE
  }

  # Setting default for colors
  if (!is.null(modx) && !is.numeric(d[[modx]])) {
    facmod <- TRUE
    if (is.character(d[[modx]])) {d[[modx]] <- factor(d[[modx]])}
  } else if (force.cat == FALSE | is.null(modx)) {
    facmod <- FALSE
  } else if (!is.null(modx)) {
    facmod <- TRUE
  }

  # Treat binary numeric moderators as quasi-categorical
  if (!is.null(modx) && length(unique(d[[modx]])) == 2) {
    if (is.null(modx.values)) {modx.values <- sort(unique(d[[modx]]))}
  }

  # Fix character mod2 as well
  if (!is.null(mod2) && !is.numeric(d[[mod2]])) {
    facmod2 <- TRUE
    if (is.character(d[[mod2]])) {d[[mod2]] <- factor(d[[mod2]])}
  } else if (force.cat == FALSE | is.null(mod2)) {
    facmod2 <- FALSE
  } else if (!is.null(mod2)) {
    facmod2 <- TRUE
  }

  # Treat binary numeric moderators as quasi-categorical
  if (!is.null(mod2) && length(unique(d[[mod2]])) == 2) {
    if (is.null(mod2.values)) {mod2.values <- sort(unique(d[[mod2]]))}
  }


  # Get the formula from lm object if given
  formula <- get_formula(model, ...)

  # Pulling the name of the response variable for labeling
  resp <- jtools::get_response_name(model, ...)

  # Create a design object
  design <- if (inherits(model, "svyglm")) {
    model$survey.design
  } else {
    NULL
  }

  # Warn user if interaction term is absent
  if (!check_interactions(formula, c(pred, modx, mod2))) {
    warn_wrap(paste(c(pred, modx, mod2), collapse = " and "),
              " are not included in an interaction with one another in the
              model.")
  }

### Getting moderator values ##################################################

  if (facpred == TRUE) {

    pred.values <- mod_vals(d = d, modx = pred, modx.values = pred.values,
                         survey = survey, weights = weights,
                         design = design,
                         modx.labels = pred.labels, is.mod2 = TRUE,
                         facet.modx = facet.modx, add.varname = FALSE)
    pred.labels <- names(pred.values)

  }

  if (!is.null(modx)) {

    modxvals2 <- mod_vals(d = d, modx = modx, modx.values = modx.values,
                          survey = survey, weights = weights,
                          design = design,
                          modx.labels = modx.labels, any.mod2 = !is.null(mod2),
                          facet.modx = facet.modx, force.cat = force.cat)
    modx.labels <- names(modxvals2)

  } else {

    modxvals2 <- NULL

  }

  if (!is.null(mod2)) {

    mod2vals2 <- mod_vals(d = d, modx = mod2, modx.values = mod2.values,
                          survey = survey, weights = weights,
                          design = design,
                          modx.labels = mod2.labels, any.mod2 = !is.null(mod2),
                          is.mod2 = TRUE, force.cat = force.cat)
    mod2.labels <- names(mod2vals2)

  } else {

    mod2vals2 <- NULL

  }

### Drop unwanted factor levels ###############################################


  if (facpred == TRUE && !is.numeric(d[[pred]])) {

    d <- drop_factor_levels(d = d, var = pred, values = pred.values,
                            labels = pred.labels)

  }

  if (facmod == TRUE && !is.numeric(d[[modx]])) {

    d <- drop_factor_levels(d = d, var = modx, values = modxvals2,
                            labels = modx.labels)

  }

  if (facmod2 == TRUE && !is.numeric(d[[mod2]])) {

    d <- drop_factor_levels(d = d, var = mod2, values = mod2vals2,
                            labels = mod2.labels)

  }

#### Creating predicted frame #################################################

  if (facpred == TRUE) {
    pred.predicted <- levels(factor(d[[pred]]))
  } else {
    pred.predicted <- seq(from = min(d[[pred]], na.rm = TRUE),
                          to = max(d[[pred]], na.rm = TRUE),
                          length.out = preds.per.level)
  }

  if (!is.null(modx)) {
    num_combos <- length(modxvals2)
    combos <- expand.grid(modxvals2)
    names(combos) <- modx
  } else {
    num_combos <- 1
  }
  if (!is.null(mod2)) {
    num_combos <- nrow(expand.grid(modxvals2, mod2vals2))
    combos <- expand.grid(modxvals2, mod2vals2)
    names(combos) <- c(modx, mod2)
  }

  pms <- list()

  for (i in seq_len(num_combos)) {

    at_list <- list()
    if (!is.null(modx)) {
      at_list[[modx]] <- combos[i, modx]
    }
    if (!is.null(mod2)) {
      at_list[[mod2]] <- combos[i, mod2]
    }

    if (!is.null(at)) {
      at_list[names(at)] <- at
    }

    suppressMessages({pms[[i]] <- jtools::make_predictions(
        model = model, data = d, pred = pred, pred.values = pred.predicted,
        at = at_list, set.offset = set.offset, center = centered,
        interval = interval, outcome.scale = outcome.scale, ...
    )})
    # only looking for completeness in these variables
    check_vars <- all.vars(get_formula(model, ...)) %just% names(pms[[i]])
    pms[[i]] <-
      pms[[i]][complete.cases(pms[[i]][check_vars]), ]
  }

  if (off == TRUE) {
    msg_wrap("Outcome is based on a total of ", set.offset, " exposures.")
  }

  pm <- do.call("rbind", pms)

  # Do partial residuals if requested
  if (partial.residuals == TRUE) {
    suppressMessages({
      d <- partialize(model, vars = c(pred, modx, mod2), center = centered,
                      data = d, scale = outcome.scale, set.offset = set.offset,
                      ...)
    })
  }

  ## Prep original data for splitting into groups ##
  if (!is.null(modx)) {
    d <- split_int_data(d = d, modx = modx, mod2 = mod2,
                        linearity.check = linearity.check | facet.modx,
                        modx.values = modx.values,
                        modxvals2 = modxvals2, mod2.values = mod2.values,
                        mod2vals2 = mod2vals2, facmod = facmod,
                        facmod2 = facmod2)
  }

  # Labels for values of moderator
  if (!is.null(modx) && !is.numeric(d[[modx]])) {
    pm[[modx]] <- factor(pm[[modx]], levels = modxvals2, labels = modx.labels)
  }
  if (facmod == TRUE) {
    d[[modx]] <- factor(d[[modx]], levels = modxvals2, labels = modx.labels)
  }
  if (!is.null(modx)) {
    if (is.numeric(d[[modx]])) {
      pm$modx_group <- factor(pm[[modx]], levels = modxvals2,
                              labels = modx.labels)
    } else {
      pm$modx_group <- factor(pm[[modx]], levels = modx.labels)
    }
  }

  # Setting labels for second moderator
  if (!is.null(mod2)) {

    # Convert character moderators to factor
    if (!is.numeric(d[[mod2]])) {
      d[[mod2]] <- factor(d[[mod2]], levels = mod2vals2, labels = mod2.labels)
      pm[[mod2]] <- factor(pm[[mod2]], levels = mod2vals2, labels = mod2.labels)
      pm$mod2_group <- pm[[mod2]]
    } else {
      pm$mod2_group <- factor(pm[[mod2]], levels = mod2vals2,
                              labels = mod2.labels)
    }

  }

  # Dealing with transformations of the dependent variable
  # Have to make sure not to confuse situations with brmsfit objects and
  # distributional DVs
  if (resp %nin% names(d) & "dpar" %nin% names(list(...))) {
    trans_name <- as.character(deparse(formula[[2]]))
    d[[trans_name]] <- eval(formula[[2]], d)
  }

  out <- list(predicted = pm, original = d)
  out <- structure(out, resp = resp, facmod = facmod,
              pred.values = pred.values, pred.labels = pred.labels,
              modxvals2 = modxvals2, modx.labels = modx.labels,
              mod2vals2 = mod2vals2, mod2.labels = mod2.labels,
              facet.modx = facet.modx)
  return(out)

}

split_int_data <- function(d, modx, mod2, linearity.check, modx.values,
                           modxvals2, mod2.values, mod2vals2, facmod, facmod2) {
  # For numeric, non-binary moderators...
  if (facmod == FALSE &
        !(length(unique(d[[modx]])) == 2 & length(modxvals2) == 2)) {

    # Use ecdf function to get quantile of the modxvals
    mod_val_qs <- ecdf(d[[modx]])(sort(modxvals2))

    # Now I am going to split the data in a way that roughly puts each modxval
    # in the middle of each group. mod_val_qs is a vector of quantiles for each
    # modxval, so I will now build a vector of the midpoint between each
    # neighboring pair of quantiles — they will become the cutpoints for
    # splitting the data into groups that roughly correspond to the modxvals
    cut_points <- c() # empty vector
    # Iterate to allow this to work regardless of number of modxvals
    for (i in 1:(length(modxvals2) - 1)) {

      cut_points <- c(cut_points, mean(mod_val_qs[i:(i + 1)]))

    }

    # Add Inf to both ends to encompass all values outside the cut points
    cut_points <- c(-Inf, quantile(d[[modx]], cut_points, na.rm = TRUE), Inf)

    # Create variable storing this info as a factor
    d["modx_group"] <- cut(d[[modx]], cut_points,
                           labels = names(sort(modxvals2)))

    if (!is.null(modx.values) && modx.values[1] == "terciles") {
      d$modx_group <- factor(cut2(d[[modx]], g = 3, levels.mean = TRUE),
                             labels = c(paste("Lower tercile of", modx),
                                        paste("Middle tercile of", modx),
                                        paste("Upper tercile of", modx)))
    }

  } else {

    d["modx_group"] <- factor(d[[modx]], levels = modxvals2,
                              labels = names(modxvals2))

  }

  if (!is.null(mod2)) {
    if (facmod2 == FALSE &
        !(length(unique(d[[mod2]])) == 2 & length(mod2vals2) == 2)) {

      mod_val_qs <- ecdf(d[[mod2]])(sort(mod2vals2))


      cut_points2 <- c()
      for (i in 1:(length(mod2vals2) - 1)) {

        cut_points2 <- c(cut_points2, mean(mod_val_qs[i:(i + 1)]))

      }

      cut_points2 <- c(-Inf, quantile(d[[mod2]], cut_points2, na.rm = TRUE),
                       Inf)

      d["mod2_group"] <- cut(d[[mod2]], cut_points2,
                          labels = names(sort(mod2vals2)))

      if (!is.null(mod2.values) && mod2.values[1] == "terciles") {
        d$mod2_group <- factor(cut2(d[[mod2]], g = 3, levels.mean = TRUE),
                               labels = c(paste("Lower tercile of", mod2),
                                          paste("Middle tercile of", mod2),
                                          paste("Upper tercile of", mod2)))
      }

    } else if (facmod2 == TRUE) {

      d["mod2_group"] <- factor(d[[mod2]], levels = mod2vals2,
                                labels = names(mod2vals2))

    }
  }

  return(d)

}

drop_factor_levels <- function(d, var, values, labels) {
  # Need to save the rownames because of tibble's stupidity
  the_row_names <- rownames(d)
  the_row_names <- the_row_names[d[[var]] %in% values]
  d <- d[d[[var]] %in% values,]
  d[[var]] <- factor(d[[var]], levels = values)
  # Can't use rowname assignment method because of tibble's stupidity
  attr(d, "row.names") <- the_row_names
  return(d)

}

# get_contrasts <- function(model) {
#   form <- as.formula(formula(model))
#   as.data.frame(t(attr(terms(form), "factors")))
# }
#
# get_int_term <- function(model, vars) {
#   cons <- get_contrasts(model)
#   # Check for non-syntactic names
#   vars <- sapply(vars, bt_if_needed)
#   cons_vars <- rowMeans(cons[, vars])
#   matches <- names(cons_vars %just% 1)
#
#   if (length(matches) == 1) {
#     return(matches)
#   } else {
#     # nasty hack but I think it works; trying to isolate lowest-order match
#     lengths <- nchar(matches)
#     return(matches[which(matches == min(lengths))])
#   }
# }
#
# threeway_contrasts_continuous <- function(model, pred, modx, mod2, modx.values,
#                                           mod2.values, .vcov) {
#   # Get all term labels
#   b1_term <- pred
#   b2_term <- modx
#   b3_term <- mod2
#   b4_term <- get_int_term(model, c(pred, modx))
#   b5_term <- get_int_term(model, c(pred, mod2))
#   b6_term <- get_int_term(model, c(modx, mod2))
#   b7_term <- get_int_term(model, c(pred, modx, mod2))
#
#   # TODO: deal with this
#   combos <- expand.grid(mod2.values, modx.values)
#   names(combos) <- c(mod2, modx)
#
#   get_delta <- function(b1, b2, b3, b4, b5, b6, b7, w1, z1, w2, z2) {}
#
# }

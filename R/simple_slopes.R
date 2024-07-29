#' Perform a simple slopes analysis.
#'
#' \code{sim_slopes} conducts a simple slopes analysis for the purposes of
#' understanding two- and three-way interaction effects in linear regression.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If `"all"`, all non-focal predictors as well as
#'   the `pred` variable are centered. You
#'   may instead pass a character vector of variables to center. User can
#'   also use "none" to base all predictions on variables set at 0.
#'   The response variable, `modx`, and `mod2` variables are never
#'   centered.
#' 
#' @param at If you want to manually set the values of other variables in the 
#'   model, do so by providing a named list where the names are the variables and
#'   the list values are vectors of the values. Note that you cannot alter the
#'   values of the `pred`, `modx`, or `mod2` variables and this will take 
#'   precedence over the `centered` argument (but any variables unmentioned by
#'   `at` will be centered as specified by `centered`). For linear models,
#'   this will only change the output of the conditional intercepts.
#'
#' @param cond.int Should conditional intercepts be printed in addition to the
#'   slopes? Default is \code{FALSE}.
#'
#' @param johnson_neyman Should the Johnson-Neyman interval be calculated?
#'   Default is \code{TRUE}. This can be performed separately with
#'   \code{\link{johnson_neyman}}.
#'
#' @param jnplot Should the Johnson-Neyman interval be plotted as well? Default
#'   is \code{FALSE}.
#'
#' @param jnalpha What should the alpha level be for the Johnson-Neyman
#'   interval? Default is .05, which corresponds to a 95% confidence interval.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired
#'   number.
#'
#' @param ... Arguments passed to \code{\link{johnson_neyman}} and
#'   `summ`.
#'
#' @param v.cov A function to calculate variances for the model. Examples
#'  could be [sandwich::vcovPC()].
#'
#' @param v.cov.args A list of arguments for the `v.cov` function. For
#'  whichever argument should be the fitted model, put `"model"`.
#'
#' @inheritParams interact_plot
#'
#' @details This allows the user to perform a simple slopes analysis for the
#'   purpose of probing interaction effects in a linear regression. Two- and
#'   three-way interactions are supported, though one should be warned that
#'   three-way interactions are not easy to interpret in this way.
#'
#'   For more about Johnson-Neyman intervals, see \code{\link{johnson_neyman}}.
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
#'  \item{mods}{A list containing each regression model created to estimate
#'     the conditional coefficients.}
#'  \item{jn}{If \code{johnson_neyman = TRUE}, a list of `johnson_neyman`
#'  objects from \code{\link{johnson_neyman}}. These contain the values of the
#'  interval and the plots. If a 2-way interaction, the list will be of length
#'  1. Otherwise, there will be 1 `johnson_neyman` object for each value of
#'  the
#'  2nd moderator for 3-way interactions.}
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @inheritParams interact_plot
#' @inheritParams jtools::summ.lm
#'
#' @family interaction tools
#'
#' @seealso \code{\link{interact_plot}} accepts similar syntax and will plot the
#'   results with \code{\link[ggplot2]{ggplot}}.
#'
#'   \code{testSlopes()} from `rockchalk` performs a hypothesis test of
#'       differences and provides Johnson-Neyman intervals.
#'
#'   \code{simpleSlope()} from `pequod` performs a similar analysis.
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
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder * Illiteracy,
#'   data = as.data.frame(state.x77))
#' sim_slopes(model = fiti, pred = Murder, modx = Illiteracy)
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals)
#'
#' # 3-way with survey and factor input
#' regmodel <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals, mod2 = sch.wide)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd update getCall vcov 
#' @importFrom stats relevel contrasts
#' @import jtools
#' @export
#'

sim_slopes <- function(model, pred, modx, mod2 = NULL, modx.values = NULL,
                       mod2.values = NULL, centered = "all", at = NULL,
                       data = NULL, cond.int = FALSE, johnson_neyman = TRUE, 
                       jnplot = FALSE, jnalpha = .05, robust = FALSE,
                       digits = getOption("jtools-digits", default = 2),
                       pvals = TRUE, confint = FALSE, ci.width = .95,
                       cluster = NULL, modx.labels = NULL, mod2.labels = NULL,
                       v.cov = NULL, v.cov.args = NULL, ...) {

  # Capture extra arguments
  dots <- list(...)
  if (length(dots) > 0) {
    # Backwards compatibility from when these arguments had different names
    if ("modxvals" %in% names(dots)) {
      modx.values <- dots$modxvals
    }
    if ("mod2vals" %in% names(dots)) {
      mod2.values <- dots$mod2vals
    }
  }

  # Evaluate the modx, mod2, pred args
  pred <- as_name(enquo(pred))
  modx <- enquo(modx)
  modx <- if (quo_is_null(modx)) {NULL} else {as_name(modx)}
  mod2 <- enquo(mod2)
  mod2 <- if (quo_is_null(mod2)) {NULL} else {as_name(mod2)}

  # Warn user if interaction term is absent
  if (!check_interactions(as.formula(formula(model)), c(pred, modx, mod2))) {
    warn_wrap("Automated checks indicate that ", 
              paste(c(pred, modx, mod2), collapse = " and "),
              " may not be included in an interaction with one another in the 
               model. Double-check to ensure the model is specified correctly.
              This message may be a false positive.")
  }

  if (length(dots) > 0) { # See if there were any extra args
    # Check for deprecated arguments
    ss_dep_check("sim_slopes", dots)

    # Get j_n args from dots
    if (johnson_neyman == TRUE) {
      jn_arg_names <- names(formals("johnson_neyman"))
      if (any(names(dots) %in% jn_arg_names)) {
        jn_args <- list()
        for (n in names(dots)[which(names(dots) %in% jn_arg_names)]) {
          jn_args[[n]] <- dots[[n]]
        }
      }
    }

    if ("robust.type" %in% names(dots)) {
      warn_wrap("The robust.type argument is deprecated. Please specify the
       type as the value for the 'robust' argument instead.", call. = FALSE)
      robust <- dots$robust.type
    }
  }

  # Create object to return
  ss <- list()

  ss <- structure(ss, digits = digits)

  d <- get_data(model)
  # see if jtools::summ() knows about this model
  has_summ <- check_method(summ, model)
  if (is_survey <- inherits(model, "svyglm")) {
    design <- model$survey.design
  } else {design <- NULL}
  # Which variables are factors?
  facvars <- names(d)[!unlist(lapply(d, is.numeric))]
  modx.factor <- modx %in% facvars
  fvars <- names(d)

  # Need to deal with possibility pred will appear in coefficient table with 
  # backticks because it is non-syntactic
  pred_names <- c(pred, bt(pred))
  pred_factor <- FALSE
  # Check for factor predictor
  if (is.factor(d[[pred]]) || is.character(d[[pred]])) {
    # Need to know what the coefficients will be called (name + level)
    if (is.factor(d[[pred]])) {
      pred_names <- 
        c(sapply(pred_names, function(x) {paste0(x, colnames(contrasts(d[[pred]])))}))
    } else {
      pred_names <- 
        c(sapply(pred_names, function(x) paste0(x, ulevels(d[[pred]]))))
    }
    pred_factor <- TRUE
  } else if (is.logical(d[[pred]])) {
    pred_names <- c(sapply(pred_names, function (x) paste0(x, c(FALSE, TRUE))))
  }
  # Only keep the ones represented among the coefficients
  if (!is.null(attr(class(model), "package")) && "lme4" %in%
        attr(class(model), "package")) {
    pred_names <- pred_names %just% names(lme4::fixef(model))
  } else {
    pred_names <- pred_names %just% names(coef(model))
  }

  if (is.null(pred_names) || length(pred_names) == 0) {
    pred_names <- c(pred, bt(pred))
    if (pred_factor) {
    # Need to know what the coefficients will be called (name + level)
      if (is.factor(d[[pred]])) {
        pred_names <- 
          c(sapply(pred_names, function(x) {paste0(x, colnames(contrasts(d[[pred]])))}))
      } else {
        pred_names <- 
          c(sapply(pred_names, function(x) paste0(x, ulevels(d[[pred]]))))
      }
    }
    
    # use tidy on the model to get the term names
    tidied <- try(generics::tidy(model), silent = TRUE)
    # if there was no method, see if it's because it's a mixed model
    if (inherits(tidied, "try-error")) {
      if (rlang::is_installed("broom.mixed")) {
        tidied <- try(broom.mixed::tidy(model), silent = TRUE)
      }
    }
    # If that doesn't fix, see if we can kludge with summ()
    if (inherits(tidied, "try-error")) {
      if (has_summ) {
        pred_names <- pred_names %just% rownames(summ(model)$coeftable)
      } else { # No summ method? Have to bail out here...
        stop_wrap("tidy() could not find a method for this kind of model. If you 
          are using a package like glmmTMB or other mixed modeling packages, 
          install and load the broom.mixed package and try again.")
      }
    } else {
      pred_names <- pred_names %just% tidied$term
    }

    if (length(pred_names) == 0) {
      stop_wrap("Could not find the focal predictor in the model. If it was
                 transformed (e.g. logged or turned into a factor), put the
                 transformation into the 'pred' argument. For example,
                 pred = '(x)'.")
    }
    }

  wname <- get_weights(model, d)$weights_name
  wts <- get_weights(model, d)$weights
  offname <- get_offset_name(model)
  # Need the raw variable name from the LHS
  resp <- all.vars(as.formula(paste("~", (get_response_name(model)))))

  # Saving key arguments as attributes of return object
  ss <- structure(ss, resp = resp, modx = modx, mod2 = mod2, pred = pred,
                  cond.int = cond.int)

### Centering #################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[!(facvars %in% c(pred, resp, modx, mod2, wname, offname))]

  # Use utility function shared by all interaction functions
  c_out <- center_ss(d = d, weights = wts, facvars = facvars,
              fvars = fvars, pred = pred,
              resp = resp, modx = modx, survey = is_survey,
              design = design, mod2 = mod2, wname = wname,
              offname = offname, centered = centered, at = at)

  design <- c_out$design
  d <- c_out$d
  fvars <- c_out$fvars
  facvars <- c_out$facvars

### Getting moderator values ##################################################

  modxvals2 <- mod_vals(d, modx, modx.values, is_survey, wts, design,
                        modx.labels = modx.labels,
                        any.mod2 = !is.null(mod2), sims = TRUE)

  if ((pred_factor || !is.numeric(d[[modx]])) && johnson_neyman == TRUE) {
        warn_wrap("Johnson-Neyman intervals are not available for factor
                   predictors or moderators.", call. = FALSE)
        johnson_neyman <- FALSE
  }

  # Now specify def or not (for labeling w/ print method)
  if (is.character(modx.values) || is.null(modx.values) || !is.null(modx.labels)) {

    ss <- structure(ss, def = TRUE)

  } else {

    ss <- structure(ss, def = FALSE)

  }

  # Don't want def = TRUE for factors even though they are character
  if (!is.numeric(d[[modx]])) {ss <- structure(ss, def = FALSE)}

  if (!is.null(mod2)) {

    if (is.numeric(d[[mod2]])) {
      mod2vals2 <- mod_vals(d, mod2, mod2.values, is_survey, wts, design,
                            modx.labels = mod2.labels, any.mod2 = !is.null(mod2),
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
    if (is.character(mod2.values) || is.null(mod2.values) || !is.null(mod2.labels)) {

      ss <- structure(ss, def2 = TRUE)

    } else {

      ss <- structure(ss, def2 = FALSE)

    }

    # Don't want def = TRUE for factors even though they are character
    if (!is.numeric(d[[mod2]])) {ss <- structure(ss, def2 = FALSE)}

  } else {
    mod2vals2 <- NULL
    if (!modx.factor) {
      modxvals2 <- rev(modxvals2)
    }
  }

#### Fit models ##############################################################

  # Since output columns are conditional, I call summ here to see what they will
  # be. I set vifs = FALSE to make sure it isn't fit due to user options.
  # kludge for panelr compatibility
  model_pkg <- attr(class(model), "package") 
  if (!is.null(model_pkg) && model_pkg == "panelr") {
    has_summ <- FALSE
  }
  tcol <- try(colnames(summary(model)$coefficients)[3], silent = TRUE)
  if (!is.null(tcol) && !inherits(tcol, "try-error")) {
    tcol <- gsub("value", "val.", tcol)
    if (tcol == "df") tcol <- "t val." # kludge for lmerModTest
    which.cols <- c("Est.", "S.E.", unlist(make_ci_labs(ci.width)), tcol)
    if (pvals == TRUE) {which.cols <- c(which.cols, "p")}
  } else {
    which.cols <- NULL
  }
  if (has_summ) {
    the_col_names <- colnames(summ(model, confint = TRUE, ci.width = ci.width,
                                vifs = FALSE, which.cols = which.cols,
                                pvals = pvals, ...)$coeftable)
  } else {
    the_col_names <- c("estimate", "std.error", "statistic", "p.value")
    if (confint) {c(the_col_names, "conf.low", "conf.high")}
  }
                              


  # Need to make a matrix filled with NAs to store values from looped
  # model-making
  holdvals <- rep(NA, length(modxvals2) * (length(the_col_names) + 1) * 
                    length(pred_names))
  retmat <- as.data.frame(matrix(holdvals,
    nrow = length(modxvals2) * length(pred_names)))

  # Create a list to hold Johnson-Neyman objects
  jns <- list()

  # Value labels
  colnames(retmat) <- c(paste("Value of ", modx, sep = ""), the_col_names)

  # Create another matrix to hold intercepts (no left-hand column needed)
  holdvals <- rep(NA, length(modxvals2) * ncol(retmat))
  retmati <- as.data.frame(matrix(holdvals, nrow = length(modxvals2)))
  colnames(retmati) <- colnames(retmat)

  mod2val_len <- length(mod2vals2)
  if (mod2val_len == 0) {mod2val_len <- 1}
  modxval_len <- length(modxvals2)

  # Make empty list to put actual models into
  mods <- rep(list(NA), times = mod2val_len)

  # Make empty list to hold above list if 2nd mod used
  if (!is.null(mod2)) {

    # Make empty list to put each matrix into
    mats <- rep(list(NA), times = mod2val_len)
    imats <- rep(list(NA), times = mod2val_len)

  }

  # Looping through (perhaps non-existent) second moderator values
  for (j in seq_len(mod2val_len)) {

    # We don't want to do the J-N interval with the 1st moderator adjusted,
    # so we do it here. Requires an extra model fit.

    # Creating extra "copy" of model frame to change for model update
    if (is_survey == FALSE) {
      dt <- d
    } else {
      # Create new design to modify
      designt <- design

      # Create new df to modify
      dt <- d
    }

    if (!is.null(mod2)) { # We *do* need to adjust the 2nd moderator for J-N

      # The moderator value-adjusted variable
      if (is.numeric(dt[[mod2]])) {
        dt[[mod2]] <- dt[[mod2]] - mod2vals2[j]
      } else {
        dt[[mod2]] <- factor(dt[[mod2]], ordered = FALSE)
        dt[[mod2]] <- relevel(dt[[mod2]], ref = as.character(mod2vals2[j]))
        dt[[mod2]] <- stats::C(dt[[mod2]], "contr.treatment")
      }

    }

    # Update design
    if (is_survey == TRUE) {
      designt$variables <- dt
      # Update model
      ## Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- designt
      call[[1]] <- survey::svyglm
      newmod <- eval(call)
    } else {
      # Creating the model
      newmod <- j_update(model, data = dt)
    }

    # Getting SEs, robust or otherwise
    if (robust != FALSE && is.null(v.cov)) {
      # For J-N
      covmat <- get_robust_se(newmod, robust, cluster, dt)$vcov
    } else if (is.null(v.cov)) {
      # For J-N
      covmat <- vcov(newmod)
    } else {
      vcovargs <- v.cov.args
      vcovargs[[which(sapply(vcovargs, function(x) length(x[[1]]) == 1 &&
                               x[[1]] == "model"))]] <-
        newmod
      covmat <- do.call(v.cov, vcovargs)
    }

    # if (robust == FALSE & is.null()) {covmat <- NULL}

    if (johnson_neyman == TRUE) {
      args <- list(newmod, pred = substitute(pred), modx = substitute(modx),
                   vmat = covmat, plot = jnplot, alpha = jnalpha,
                   digits = digits)
      if (exists("jn_args")) {args <- as.list(c(args, jn_args))}
      jn <- do.call("johnson_neyman", args)
    } else {

      jn <- NULL

    }

    if (j != 0) {
        jns[[j]] <- jn
    }

  # Looping so any amount of moderator values can be used
  for (i in seq_along(modxvals2)) {

    dt <- d

    # Create new design to modify
    if (is_survey == TRUE) {designt <- design}

    # The moderator value-adjusted variable
    if (is.numeric(dt[[modx]])) {
      dt[[modx]] <- dt[[modx]] - modxvals2[i]
    } else {
      dt[[modx]] <- factor(dt[[modx]], ordered = FALSE)
      dt[[modx]] <- relevel(dt[[modx]], ref = as.character(modxvals2[i]))
      dt[[modx]] <- stats::C(dt[[modx]], "contr.treatment")
    }

    if (!is.null(mod2)) {

      # The moderator value-adjusted variable
      if (is.numeric(dt[[mod2]])) {
        dt[[mod2]] <- dt[[mod2]] - mod2vals2[j]
      } else {
        dt[[mod2]] <- factor(dt[[mod2]], ordered = FALSE)
        dt[[mod2]] <- relevel(dt[[mod2]], ref = as.character(mod2vals2[j]))
        dt[[mod2]] <- stats::C(dt[[mod2]], "contr.treatment")
      }


    }

    # Creating the model
    if (is_survey == TRUE) {
      # Update design
      designt$variables <- dt

      # Update model
      ## Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- designt
      call[[1]] <- survey::svyglm
      newmod <- eval(call)
    } else {
      newmod <- j_update(model, data = dt)
    }

    # Getting SEs, robust or otherwise
    if (robust != FALSE) {
      if (!is.null(v.cov)) {
        vcovargs <- v.cov.args
        vcovargs[[which(sapply(vcovargs, function(x) length(x[[1]]) == 1 &&
                                 x[[1]] == "model"))]] <-
          newmod
        covmat <- do.call(v.cov, vcovargs)
      } else {
        covmat <- NULL
      }
      # Use summ to get the coefficients
      if (has_summ) {
        sum <- summ(newmod, robust = robust, model.fit = FALSE,
                    confint = TRUE, ci.width = ci.width, vifs = FALSE,
                    cluster = cluster, which.cols = which.cols, pvals = pvals,
                    vcov = covmat, ...)
      } else {
        sum <- generics::tidy(newmod, conf.int = TRUE, conf.level = ci.width)
      }
    } else {
      if (is.null(v.cov)) {
        # For J-N
        covmat <- vcov(newmod)
      } else {
        vcovargs <- v.cov.args
        vcovargs[[which(sapply(vcovargs, function(x) length(x[[1]]) == 1 &&
                                 x[[1]] == "model"))]] <-
          newmod
        covmat <- do.call(v.cov, vcovargs)
      }
      if (has_summ) {
        sum <- summ(newmod, model.fit = FALSE, confint = TRUE,
                  ci.width = ci.width, vifs = FALSE,
                  which.cols = which.cols, pvals = pvals, vcov = covmat, ...)
      } else {
        sum <- generics::tidy(newmod, conf.int = TRUE, conf.level = ci.width)
      }
    }
    if (has_summ) {
      summat <- sum$coeftable
      if (pvals == FALSE) {summat <- summat[,colnames(summat) %nin% "p"]}
      slopep <- summat[pred_names, , drop = FALSE]
      intp <- summat["(Intercept)", ]
    } else {
      summat <- sum
      if (pvals == FALSE) {summat <- summat %not% "p.value"}
      slopep <- summat[summat$term %in% pred_names, , drop = FALSE]
      intp <- summat[summat$term == "(Intercept)", ]
    }

    # Have to account for variable amount of rows needed due to factor 
    # predictors
    rows <- split(1:nrow(retmat),
                    ceiling(seq_along(1:nrow(retmat))/length(pred_names)))
    # if (length(pred_names) == 1) {rows <- 1:nrow(retmat)}

    retmat[rows[[i]],1] <- modxvals2[i]
    retmat[rows[[i]],2:ncol(retmat)] <- slopep[, colnames(retmat)[-1]]

    retmati[i,1] <- modxvals2[i]
    retmati[i,2:ncol(retmat)] <- intp[colnames(retmati)[-1]]

    if (length(pred_names) > 1) {
      pred_coefs <- rep(rownames(slopep), length(modxvals2))
    } else {pred_coefs <- NULL}

    mods[[i + (j - 1) * modxval_len]] <- newmod

  }

    if (!is.null(mod2)) {

      mats[[j]] <- retmat
      imats[[j]] <- retmati

      # Now reset the return matrices
      holdvals <- rep(NA, length(modxvals2) * ncol(retmat) * length(pred_names))
      retmat <- as.data.frame(
        matrix(holdvals, nrow = length(modxvals2) * length(pred_names))
      )

      # Create another matrix to hold intercepts (no left-hand column needed)
      holdvals <- rep(NA, length(modxvals2) * ncol(retmat))
      retmati <- as.data.frame(matrix(holdvals, nrow = length(modxvals2)))

      # Value labels
      colnames(retmat) <-
        c(paste("Value of ", modx, sep = ""), colnames(slopep))
      colnames(retmati) <-
        c(paste("Value of ", modx, sep = ""), colnames(slopep))

    }

  } # end mod2 loop


    ss <- structure(ss, modx.values = modxvals2, robust = robust,
                    cond.int = cond.int, johnson_neyman = johnson_neyman,
                    jnplot = jnplot, jns = jns, confint = confint,
                    ci.width = ci.width, pred_names = pred_names)

    ss$mods <- mods
    ss$jn <- jns

    if (!is.null(mod2)) {
      if (!is.null(pred_coefs)) {
        for (i in 1:length(mats)) {
          mats[[i]]["Coef."] <- pred_coefs
          ocols <- names(mats[[i]]) 
          mats[[i]] <- mats[[i]][c(ocols[1], "Coef.", ocols[-1] %not% "Coef.")]

          imats[[i]]["Coef."] <- "(Intercept)"
          ocols <- names(imats[[i]]) 
          imats[[i]] <- imats[[i]][c(ocols[1], "Coef.", ocols[-1] %not% "Coef.")]
        }
      }
      ss$slopes <- mats
      ss$ints <- imats
      ss <- structure(ss, mod2.values = mod2vals2)
    } else {
      if (!is.null(pred_coefs)) {
        retmat["Coef."] <- pred_coefs
        ocols <- names(retmat) 
        retmat <- retmat[c(ocols[1], "Coef.", ocols[-1] %not% "Coef.")]

        retmati["Coef."] <- "(Intercept)"
        ocols <- names(retmati) 
        retmati <- retmati[c(ocols[1], "Coef.", ocols[-1] %not% "Coef.")]
      }
      ss$slopes <- retmat
      ss$ints <- retmati
    }

    class(ss) <- "sim_slopes"

#### build jnplot for 3-way interactions ######################################

  # If 3-way interaction and the user has `cowplot`, here's where we make the
  # final output
  if (!is.null(mod2) & johnson_neyman == TRUE & jnplot == TRUE) {

    # plots <- as.list(rep(NA, length(mod2vals) + 2))
    plots <- as.list(rep(NA, length(mod2.values)))
    the_legend <- NULL

    for (j in seq_along(jns)) {

    # Tell user we can't plot if they don't have cowplot installed
    if (jnplot == TRUE & !is.null(mod2) &
      !requireNamespace("cowplot", quietly = TRUE)) {

      msg <- wrap_str("To plot Johnson-Neyman plots for 3-way interactions,
                       you need the cowplot package.")
      warning(msg)
      jnplot <- FALSE # No more attempts at plotting

    } else if (jnplot == TRUE & !is.null(mod2)) {

      if (is.null(the_legend)) {
        # We save the legend the first time around to use w/ cowplot
        the_legend <-
          cowplot::get_legend(jns[[j]]$plot +
             theme_apa(legend.font.size = 8) +
               ggplot2::theme(legend.position = "bottom"))

        # Now we get rid of it for the actual plotting of the first plot
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")

      } else {
        # For each subsequent plot, we don't need to save the legend,
        # just need to get rid of it
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")
      }

      # Add a label for cowplot
      mod2lab <- names(mod2vals2)[j]
      if (is.null(mod2lab)) {mod2lab <- mod2vals2[j]}
      if (is.null(mod2.labels)) {mod2lab <- paste(mod2, "=", mod2lab)}
      jns[[j]]$plot <-
        jns[[j]]$plot + ggplot2::ggtitle(mod2lab) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 11))

      # Add the plot to the plot list at whatever the current end is
      index <- j
      plots[[index]] <- jns[[j]]$plot

    }

  }

  if (length(plots) %% 2 == 1) {

    plots[[length(plots) + 1]] <- the_legend
    just_plots <- cowplot::plot_grid(plotlist = plots, align = "auto",
                                         ncol = 2, vjust = 0, scale = 1)
    with_legend <- just_plots

  } else {

    just_plots <- cowplot::plot_grid(plotlist = plots, ncol = 2)
    with_legend <- cowplot::plot_grid(just_plots, the_legend,
                                      rel_heights = c(1,.1), nrow = 2)

  }

  # Now we put it all together--vjust is at a non-default level
  ss$jnplot <- with_legend

  } else if (johnson_neyman == TRUE & jnplot == TRUE) {

    ss$jnplot <- jns[[1]]$plot

  }

  if (jnplot == FALSE | johnson_neyman == FALSE) {

    ss$jnplot <- NULL

  }

  return(ss)

}


#### PRINT METHOD ############################################################

#' @export
#' @importFrom cli cat_rule rule

print.sim_slopes <- function(x, ...) {

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

    # If we're using second moderator, need to make things make sense
    # to inner loop
    if (!is.null(x$mod2)) {

      m <- NULL
      m$slopes <- as.data.frame(ss$slopes[[j]], stringsAsFactors = FALSE)
      fac_pred <- "Coef." %in% names(m$slopes)
      pred_names <- if (fac_pred) {unique(m$slopes[["Coef."]])} else {x$pred}
      if (x$confint == FALSE) {
        m$slopes <-
          m$slopes[names(m$slopes) %nin% unlist(make_ci_labs(x$ci.width))]
      }
      m$ints <- as.data.frame(ss$ints[[j]], stringsAsFactors = FALSE)
      if (x$confint == FALSE) {
        m$ints <- m$ints[names(m$ints) %nin% unlist(make_ci_labs(x$ci.width))]
      }

      if (!inherits(x$mod2.values, "character")) {
        x$mod2.values <- format(x$mod2.values, nsmall = x$digits)
      }

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator) ",
                               "= ", x$mod2.values[j]), line = "bar8"), "\n\n")
      } else {
        # If the user went with default +/- SD or used a factor variable,
        # we use the labels
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator)", " = ",
                 x$mod2.values[j], " (", names(x$mod2.values)[j], ")"),
                 line = "bar8"), "\n\n")
      }


      if (x$johnson_neyman == TRUE) {

        # For 3-way interactions, we don't want each plot being printed
        attributes(x$jns[[j]])$plot <- FALSE
        print(x$jns[[j]])

      }

    } else {
      m <- ss
      m <- NULL
      m$slopes <- as.data.frame(ss$slopes, stringsAsFactors = FALSE)
      fac_pred <- "Coef." %in% names(m$slopes)
      pred_names <- if (fac_pred) {unique(m$slopes[["Coef."]])} else {x$pred}
      if (x$confint == FALSE) {
        m$slopes <-
          m$slopes[names(m$slopes) %nin% unlist(make_ci_labs(x$ci.width))]
      }
      m$ints <- as.data.frame(ss$ints, stringsAsFactors = FALSE)
      if (x$confint == FALSE) {
        m$ints <- m$ints[names(m$ints) %nin% unlist(make_ci_labs(x$ci.width))]
      }

      if (x$johnson_neyman == TRUE) {
        print(x$jns[[j]])
      }

    }

    # Clearly label simple slopes
    cli::cat_line(
      cli::style_bold(cli::style_underline("SIMPLE SLOPES ANALYSIS")),
      "\n"
    )

    for (i in seq_along(x$modx.values)) {

      if (!inherits(x$modx.values, "character")) {
        x$modx.values <- format(x$modx.values, nsmall = x$digits)
      }

      rows <- split(1:nrow(m$slopes),
                    ceiling(seq_along(1:nrow(m$slopes))/length(pred_names)))
      slopes <- m$slopes[rows[[i]], 2:ncol(m$slopes)]

      # Handle automatic labels
      if (x$def == TRUE) {
        modx_label <- paste0(x$modx.values[i], " (", names(x$modx.values)[i],
                             ")")
      } else {
        modx_label <- paste0(x$modx.values[i])
      }

      # Print conditional intercept
      if (x$cond.int == TRUE | fac_pred == TRUE) {
        pred_lab <- if (fac_pred) {slopes[["Coef."]]} else {x$pred}
        cli::cat_line(cli::style_italic(paste0("When ", x$modx, " = ", modx_label, ": \n")))
        if (x$cond.int) {
          ints <- m$ints[i,2:ncol(m$slopes)]
          slopes <- as.data.frame(rbind(slopes, ints))
          rownames(slopes) <- c(paste0("Slope of ", pred_lab), 
                                "Conditional intercept")
        } else {
          rownames(slopes) <- paste0("Slope of ", pred_lab)
        }
        
        print(md_table(slopes %not% "Coef.", digits = x$digits,
                       format = "pandoc", sig.digits = FALSE))
      } else {
        cli::cat_line(cli::style_italic(paste0("Slope of ", x$pred, " when ", x$modx, " = ",
                          modx_label, ": \n")))
        print(md_table(slopes, digits = x$digits, format = "pandoc",
                       row.names = FALSE, sig.digits = FALSE))
      }

      cat("\n")

    }
  } # end mod2 loop

  if (!is.null(x$mod2) && x$jnplot == TRUE) {
    print(ss$jnplot)
  }

}

#### alternate output formats ################################################

#' @title Tidiers for [sim_slopes()] objects.
#' @description You can use [broom::tidy()] and [broom::glance()] for "tidy"
#'  methods of storing `sim_slopes` output.
#' @param x The `sim_slopes` object
#' @param conf.level The width of confidence intervals. Default is .95 (95%).
#' @param ... Ignored.
#' @rdname sim_slopes_tidiers
#' @export

tidy.sim_slopes <- function(x, conf.level = .95, ...) {

  cols <- c("estimate", "std.error", "statistic", "p.value", "modx",
            "modx.value", "mod2", "mod2.value")
  # Figure out how many rows the data frame will be
  num_coefs <- ifelse(!is.data.frame(x$slopes),
                      yes = length(x$slopes) * nrow(x$slopes[[1]]),
                      no = nrow(x$slopes))
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
  if ("Coef." %in% names(all_slopes)) {
    base$term <- paste0(base$term, ", ", all_slopes[["Coef."]])
    base$pred.value <- attr(x, "pred_names")
  }

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

#' @rdname sim_slopes_tidiers
#' @export

glance.sim_slopes <- function(x, ...) {
  data.frame(N = length(residuals(x$mods[[1]])))
}

#' @importFrom stats nobs
#' @export

nobs.sim_slopes <- function(object, ...) {
  length(residuals(object$mods[[1]]))
}


#' @title Create tabular output for simple slopes analysis
#'
#' @description This function converts a `sim_slopes` object into a
#' `huxtable` object, making it suitable for use in external documents.
#'
#' @param x The [sim_slopes()] object.
#' @param format The method for sharing the slope and associated uncertainty.
#'  Default is `"{estimate} ({std.error})"`. See the instructions for the
#'  `error_format` argument of [jtools::export_summs()] for more on your
#'  options.
#' @param sig.levels A named vector in which the values are potential p value
#'  thresholds and the names are significance markers (e.g., "*") for when
#'  p values are below the threshold. Default is
#'  \code{c(`***` = .001, `**` = .01, `*` = .05, `#` = .1)}.
#' @param digits How many digits should the outputted table round to? Default
#'  is 2.
#' @param conf.level How wide the confidence interval should be, if it
#'  is used. .95 (95% interval) is the default.
#' @param intercept Should conditional intercepts be included? Default is
#'  whatever the `cond.int` argument to `x` was.
#' @param int.format If conditional intercepts were requested, how should
#'  they be formatted? Default is the same as `format`.
#' @param ... Ignored.
#'
#' @details
#'
#' For more on what you can do with a `huxtable`, see \pkg{huxtable}.
#'
#' @rdname as_huxtable.sim_slopes
#' @rawNamespace
#' if (getRversion() >= "3.6.0") {
#'   S3method(huxtable::as_huxtable, sim_slopes)
#' } else {
#'   export(as_huxtable.sim_slopes)
#' }

as_huxtable.sim_slopes <-  function(x, format = "{estimate} ({std.error})",
  sig.levels = c(`***` = .001, `**` = .01, `*` = .05, `#` = .1),
  digits = getOption("jtools-digits", 2), conf.level = .95,
  intercept = attr(x, "cond.int"), int.format = format, ...) {

  df <- tidy.sim_slopes(x, conf.level = conf.level)
  if (intercept == TRUE) {
    ints <- x$ints
    # Need to bind rows if there's a second moderator
    if (!is.null(attr(x, "mod2"))) {
      ints <- tibble::as_tibble(do.call("rbind", ints))
    } else {
      ints <- tibble::as_tibble(ints)
    }
    # Need to rename columns to be like tidy d.f.
    names(ints)[names(ints) %in% unlist(make_ci_labs(conf.level))] <-
      c("conf.lower", "conf.upper")
    names(ints)[names(ints) %in%
      c("Est.", "S.E.", grep("val.", names(ints), value = T), "p")] <-
      c("estimate", "std.error", "statistic", "p.value")
  } else {
    ints <- NULL
  }
  make_table(df = df, format = format, sig.levels = sig.levels,
             digits = digits, intercept = ints)

}

# Worker function for as_huxtable

make_table <- function(df, format = "{estimate} ({std.error})",
                       sig.levels = c(`***` = .001, `**` = .01, `*` = .05,
                                      `#` = .1),
                       digits = getOption("jtools-digits", 2),
                       label = "Slope of", intercept = NULL,
                       int.format = format) {

  # Get the predictor variable name
  pred.name <- attr(df, "pred")

  # Quick function to create asterisks
  return_asterisk <- function(x, levels) {
    if (is.null(levels)) {return("")}
    levels <- sort(levels)
    for (i in seq_len(length(levels))) {
      if (x < levels[i]) {return(names(levels)[i])}
    }
    return("")
  }
  # Vectorize it
  return_asterisks <- function(x, levels = c(`***` = .001, `**` = .01,
                                             `*` = .05, `#` = .1)) {
    sapply(x, return_asterisk, levels = levels)
  }

  # Append the asterisks to the format
  asts <- glue::glue_data("{return_asterisks(p.value, levels = sig.levels)}",
                          .x = df)
  # Format the numbers pre-emptively
  df <- as.data.frame(lapply(df, function(x) {
    if (is.numeric(x)) {num_print(x, digits)} else {x}
  }))

  # Deal with intercepts
  ints <- if (!is.null(intercept)) {
    # This is a non-obvious way of seeing if we have a sim_margins or slopes
    if (is.data.frame(intercept)) {
      # Format the numbers
      intercept <- as.data.frame(lapply(intercept, function(x) {
        if (is.numeric(x)) {num_print(x, digits)} else {x}
      }))
      # Use glue to format the key data
      glue::glue_data(.x = intercept, int.format)
    } else {
      # If it's sim_margins, it's just a list of numbers
      intercept
    }
  } else {NULL}

  # Create new DF with the minimal information
  df2 <- data.frame(
    # Moderator value
    modx.value = num_print(df$modx.value, digits),
    # Formatted slope
    slope = paste0(glue::glue_data(.x = df, format), asts),
    # 2nd moderator value (gets dropped later)
    mod2.value = df$mod2.value
  )

  if ("pred.value" %in% names(df)) {
    df2$pred.value <- df$pred.value
  }

  if (!is.null(ints)) {
    df2["Conditional intercept"] <- num_print(ints, digits)
    int.name <- "Conditional intercept"
  } else {
    int.name <- NULL
  }

  # Add "slope of"
  pred.name <- paste(label, pred.name)

  # Get moderator name
  modx.name <- df$modx[1]
  # Add "value of"
  modx.name <- paste("Value of", modx.name)
  # Change df2 colname to modx.name
  names(df2)[names(df2) == "modx.value"] <- modx.name
  names(df2)[names(df2) == "slope"] <- pred.name
  if ("pred.value" %in% names(df2)) {
    names(df2)[names(df2) == "pred.value"] <- "Coefficient"
  }

  # Create huxtable sans moderator 2 column
  tab <- huxtable::as_hux(df2 %not% "mod2.value" %just%
                            c("Coefficient", modx.name, int.name, pred.name))
  tab <- tab[names(tab) %just% c("Coefficient", modx.name, int.name, pred.name)]

  pred.values <- if ("Coefficient" %in% colnames(tab)) {"Coefficient"} else {NULL}

  # Align the huxtable left
  tab <- huxtable::set_align(tab, value = "left")

  col_mult <- ifelse(is.null(int.name), yes = 1, no = 2)
  col_mult <- ifelse(is.null(df2$Coefficient), no = col_mult + 1, yes = col_mult)

  # 3-way interaction handling
  if (any(!is.na(df2$mod2.value))) {
    # Get each unique value of the 2nd moderator
    mod2s <- unique(df2$mod2.value)
    # Save the number of those
    num_mod2 <- length(mod2s)
    # Get the number of moderator values per 2nd moderator values
    vals_per_mod2 <- (length(df2$mod2.value)/num_mod2)

    # Iterate through 2nd moderator values
    for (i in 0:(num_mod2 - 1)) {
      # Generate label
      lab <- paste(df$mod2[1], "=", mod2s[i + 1])
      # Get the row I'll be inserting to; it's *2 because there are two row
      # insertions each time.
      row <- (i * vals_per_mod2) + i * 2 

      # Insert row with 2nd moderator label
      tab <- huxtable::insert_row(tab, c(lab, rep(NA, col_mult)), after = row)
      # Make that row cell span both columns
      tab <- huxtable::set_colspan(tab, row + 1, 1, 2 + (col_mult - 1))
      # Align the row to the left
      tab <- huxtable::set_align(tab, row + 1, 1, "left")

      # Insert row with column labels
      if (i > 0) {
        tab <- huxtable::insert_row(tab, c(pred.values, modx.name, int.name, pred.name),
                                    after = row + 1)
      }
      # Put border below that row
      tab <- huxtable::set_bottom_border(tab, row + 2, 1:(2 + (col_mult - 1)),
                                         1)

      # Now need to format just the top row...
      # Italicize the font
      tab <- huxtable::set_italic(tab, row + 1, 1, TRUE)
      # Put a border above that row
      tab <- huxtable::set_top_border(tab, row + 1, 1:(2 + (col_mult - 1)), 1)
    }

  } else { # If no second moderator
    # Add row of column labels
    # tab <- huxtable::insert_row(tab, c(modx.name, int.name, pred.name))
    # Put a line below the column labels
    tab <- huxtable::set_bottom_border(tab, 1, 1:(2 + (col_mult - 1)), 1)
  }

  # Format the numbers
  # tab <- huxtable::set_number_format(tab, value = digits)
  # Drop the huxtable colnames
  colnames(tab) <- NULL

  tab

}

#' @export
#' @title Plot coefficients from simple slopes analysis
#' @description This creates a coefficient plot to visually summarize the
#' results of simple slopes analysis.
#' @param x A [sim_slopes()] object.
#' @param ... arguments passed to [jtools::plot_coefs()]

plot.sim_slopes <- function(x, ...) {
  # Get the plot and add better x-axis label
  p <- plot_coefs(x, ...) + ggplot2::xlab(paste("Slope of", attr(x, "pred")))

  # If there's a second moderator, format as appropriate
  if (!is.null(attr(x, "mod2"))) {
    p <- p + ggplot2::facet_wrap(mod2.term ~ ., ncol = 1, scales = "free_y",
                                 strip.position = "top")
  }

  p
}

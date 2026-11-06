# The contrast engine is model-agnostic; model methods supply the design rows.
contrast_inference <- function(L, beta, V, df, level, offset = 0) {
  estimate <- unname(drop(L %*% beta) + offset)
  variance <- unname(rowSums((L %*% V) * L))
  if (any(variance < -1e-8 * max(1, abs(variance)))) {
    stop_wrap("The covariance matrix implies negative contrast variances.")
  }
  se <- sqrt(pmax(0, variance))
  statistic <- estimate / se
  critical <- stats::qt((1 + level) / 2, df)
  out <- data.frame(estimate, se, estimate - critical * se,
                    estimate + critical * se, statistic,
                    2 * stats::pt(-abs(statistic), df), check.names = FALSE)
  names(out) <- c("Est.", "S.E.", unlist(make_ci_labs(level)),
                  if (is.finite(df)) "t val." else "z val.", "p")
  out
}

panel_slope_df <- function(df, design) {
  if (is.null(df)) df <- if (design$gaussian) "residual" else "normal"
  if (identical(df, "residual")) df <- design$df
  if (identical(df, "normal")) df <- Inf
  if (!is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0) {
    stop_wrap("df must be 'residual', 'normal', or a positive number.")
  }
  df
}

#' @export
sim_slopes.wbm <- function(model, pred, modx, mod2 = NULL, modx.values = NULL,
                           mod2.values = NULL, centered = "all", at = NULL,
                           data = NULL, cond.int = FALSE, johnson_neyman = TRUE,
                           jnplot = FALSE, jnalpha = .05, robust = FALSE,
                           digits = getOption("jtools-digits", default = 2),
                           pvals = TRUE, confint = FALSE, ci.width = .95,
                           cluster = NULL, modx.labels = NULL, mod2.labels = NULL,
                           v.cov = NULL, v.cov.args = NULL,
                           plot.scale = c("within", "original"), df = NULL, ...) {
  pred <- as_name(enquo(pred))
  modx <- as_name(enquo(modx))
  mod2 <- enquo(mod2)
  mod2 <- if (quo_is_null(mod2)) NULL else as_name(mod2)
  dots <- list(...)
  if (!is.null(dots$modxvals)) modx.values <- dots$modxvals
  if (!is.null(dots$mod2vals)) mod2.values <- dots$mod2vals
  if (!is.null(dots$robust.type)) robust <- dots$robust.type
  if (!identical(robust, FALSE) || !is.null(cluster) || !is.null(v.cov) ||
      !is.null(v.cov.args)) {
    stop_wrap("Panelr slopes use the fitted covariance matrix; custom/robust",
              " covariance options are not supported. GEE retains its fitted sandwich covariance.")
  }
  if (!is.null(dots$outcome.scale) &&
      !(dots$outcome.scale == "link" ||
        (dots$outcome.scale == "response" && family(model)$link == "identity"))) {
    stop_wrap("Panelr simple slopes are on the link scale. Response-scale",
              " derivatives for non-identity links are not yet supported.")
  }
  jn_names <- c("control.fdr", "line.thickness", "critical.t", "sig.color",
                "insig.color", "mod.range", "title", "y.label", "modx.label")
  unknown <- setdiff(names(dots), c(jn_names, "modxvals", "mod2vals",
                                   "robust.type", "outcome.scale"))
  if (length(unknown)) stop_wrap("Unsupported panelr slope options: ", paste(unknown, collapse = ", "))
  if (!is.numeric(ci.width) || length(ci.width) != 1L || !is.finite(ci.width) ||
      ci.width <= 0 || ci.width >= 1) stop_wrap("ci.width must be between 0 and 1.")
  if (!requireNamespace("panelr", quietly = TRUE) ||
      !"wb_slope_design" %in% getNamespaceExports("panelr")) {
    stop_wrap("Update panelr to version 1.1.0 or later to use contrast-based simple slopes.")
  }
  focal <- c(pred, modx, mod2)
  if (anyDuplicated(focal)) stop_wrap("The focal predictor and moderators must be different variables.")
  if (length(intersect(names(at), focal))) {
    stop_wrap("Use modx.values/mod2.values for moderators; 'at' cannot set focal variables.")
  }
  if (any(lengths(at) != 1L)) stop_wrap("Use one value per control/reference quantity in 'at'.")
  d <- if (is.null(data)) get_data(model) else data
  if (!all(focal %in% names(d))) stop_wrap("The focal variables must be present in the fitting data.")
  if (!is.numeric(d[[pred]])) stop_wrap("Panelr simple slopes currently require a numeric focal predictor.")
  if (!check_interactions(get_formula(model), focal, model = model)) {
    warn_wrap("The requested variables are not included in an interaction with one another.")
  }
  prepared <- prepare_panel_plot(model, d, pred, modx, mod2, plot.scale,
                                 modx.values, mod2.values, at)
  d <- prepared$data
  pm <- prepared$model
  context <- attr(pm, "panelr_plot")
  weights <- jtools::get_weights(pm, d)$weights
  values <- function(var, supplied, labels) {
    if (is.null(var)) return(NULL)
    if (is.null(labels) && !is.null(names(supplied))) labels <- names(supplied)
    if (is.null(supplied) && length(unique(d[[var]])) <= 2L) {
      supplied <- sort(unique(d[[var]]))
    }
    out <- mod_vals(d, var, supplied, survey = FALSE, weights = weights,
                     design = NULL, modx.labels = labels, sims = TRUE,
                     is.mod2 = TRUE, add.varname = FALSE)
    if (anyNA(out) || anyDuplicated(unname(out)) ||
        (is.numeric(out) && any(!is.finite(out)))) {
      stop_wrap("Moderator values must be distinct and nonmissing (finite for numeric moderators).")
    }
    out
  }
  mx <- values(modx, prepared$modx.values, modx.labels)
  mz <- values(mod2, prepared$mod2.values, mod2.labels)
  for (v in names(context$reference)) {
    if (!v %in% names(d)) d[[v]] <- context$reference[[v]]
  }
  baseline <- if (context$scale == "within" && pred %in% names(context$means)) 0 else mean(d[[pred]], na.rm = TRUE)
  make_grid <- function(xvalues, zvalue = NULL) {
    at_values <- at
    at_values[[modx]] <- unname(xvalues)
    if (!is.null(mod2)) at_values[[mod2]] <- unname(zvalue)
    jtools::make_new_data(pm, pred = pred, pred.values = baseline, at = at_values,
                          data = d, center = centered)
  }
  groups <- if (is.null(mod2)) list(NULL) else as.list(unname(mz))
  designs <- lapply(groups, function(z) {
    panelr::wb_slope_design(pm, pred, make_grid(mx, z), cond.int = cond.int)
  })
  df <- panel_slope_df(df, designs[[1]])
  note <- paste0("Fixed-effect slopes on the ",
                  if (designs[[1]]$link == "identity") "response scale (identity link). " else
                    paste0(designs[[1]]$link, " link scale. "),
                  if (context$scale == "within" && length(context$means))
                    "Numeric within-person predictors/moderators use deviations in original units. " else "",
                  if (is.finite(df)) paste0("t approximation with ", signif(df, 6),
                    " degrees of freedom (not Satterthwaite/Kenward-Roger). ") else "Normal-approximation tests. ",
                  prepared$caption)
  note <- paste(strwrap(note, width = 85), collapse = "\n")
  caption <- paste0("Fixed-effect slopes: ",
    if (designs[[1]]$link == "identity") "response scale (identity link)." else
      paste0(designs[[1]]$link, " link scale."), "\n",
    if (is.finite(df)) paste0("t(", signif(df, 6),
      ") approximation; not Satterthwaite/Kenward-Roger.") else
      "Normal-approximation tests.", "\n", prepared$caption)
  tables <- function(design, intercept = FALSE) {
    stats <- contrast_inference(if (intercept) design$X0 else design$L,
                                 design$beta, design$vcov, df, ci.width,
                                 offset = if (intercept) design$offset else 0)
    out <- data.frame(unname(mx), stats, check.names = FALSE)
    names(out)[1] <- paste("Value of", modx)
    out
  }
  slopes <- lapply(designs, tables)
  ints <- lapply(designs, tables, intercept = TRUE)
  jns <- list()
  if (johnson_neyman && !is.numeric(d[[modx]])) {
    warn_wrap("Johnson-Neyman intervals require a numeric moderator.")
    johnson_neyman <- FALSE
  }
  if (johnson_neyman) {
    jns <- lapply(seq_along(groups), function(i) {
      observed <- range(d[[modx]], na.rm = TRUE)
      center <- mean(observed)
      scale <- diff(observed) / 2
      if (!is.finite(scale) || scale <= 0) stop_wrap("Johnson-Neyman analysis requires moderator variation.")
      design <- panelr::wb_slope_design(pm, pred, make_grid(c(center, center + scale), groups[[i]]),
                                        check_support = FALSE, linear.moderator = modx)
      A <- design$L[1, ]
      B <- design$L[2, ] - A
      C <- rbind(A, B)
      args <- list(effect = drop(C %*% design$beta), covariance = C %*% design$vcov %*% t(C),
                    modrange = observed, center = center, scale = scale,
                    pred = pred, modx = modx, df = df, alpha = jnalpha,
                    plot = is.null(mod2) && jnplot, digits = digits,
                    caption = caption, modx.label = prepared$legend.main,
                    y.label = paste("Slope of", prepared$x.label))
      overrides <- dots[intersect(names(dots), jn_names)]
      overrides <- overrides[!vapply(overrides, is.null, logical(1))]
      args[names(overrides)] <- overrides
      do.call(contrast_johnson_neyman, args)
    })
  }
  result <- list(slopes = if (is.null(mod2)) slopes[[1]] else slopes,
                  ints = if (is.null(mod2)) ints[[1]] else ints,
                  model = model, mods = list(), jn = jns,
                  contrasts = lapply(designs, `[[`, "L"))
  attr(result, "panelr") <- list(note = note, caption = caption, scale = context$scale,
                                 reference = context$reference,
                                 pred.label = prepared$x.label,
                                 modx.label = prepared$legend.main,
                                 mod2.label = if (!is.null(mod2) && context$scale == "within" &&
                                                   mod2 %in% names(context$means))
                                   paste0(mod2, " (within-person deviation)") else mod2)
  result <- structure(result, class = "sim_slopes", digits = digits,
    pred = pred, modx = modx, mod2 = mod2, resp = get_response_name(model),
    pred_names = pred, modx.values = mx, mod2.values = mz,
    def = !is.null(modx.labels) || !is.null(names(modx.values)),
    def2 = !is.null(mod2.labels) || !is.null(names(mod2.values)),
    robust = FALSE, cond.int = cond.int, pvals = pvals,
    johnson_neyman = johnson_neyman, jnplot = jnplot && johnson_neyman,
    jns = jns, confint = confint, ci.width = ci.width, inference.df = df)
  if (!is.null(mod2) && johnson_neyman && jnplot) {
    if (!requireNamespace("cowplot", quietly = TRUE)) {
      stop_wrap("cowplot is required for a three-way Johnson-Neyman plot.")
    }
    plots <- lapply(seq_along(jns), function(i) jns[[i]]$plot +
                      ggplot2::labs(subtitle = paste(mod2, "=", mz[i])))
    result$jnplot <- cowplot::plot_grid(plotlist = plots, ncol = 1)
  }
  result
}

#' @export
sim_slopes.wbgee <- sim_slopes.wbm

#' @export
johnson_neyman.wbm <- function(model, pred, modx, vmat = NULL, alpha = .05,
                                plot = TRUE, control.fdr = FALSE,
                                line.thickness = .5, df = NULL,
                                digits = getOption("jtools-digits", 2),
                                critical.t = NULL, sig.color = "#00BFC4",
                                insig.color = "#F8766D", mod.range = NULL,
                                title = "Johnson-Neyman plot", y.label = NULL,
                                modx.label = NULL,
                                plot.scale = c("within", "original")) {
  if (!is.null(vmat)) stop_wrap("Custom covariance matrices are not supported for panelr slopes.")
  pred <- enquo(pred)
  modx <- enquo(modx)
  out <- sim_slopes(model, pred = !!pred, modx = !!modx, df = df,
                     plot.scale = plot.scale, jnalpha = alpha, digits = digits,
                     control.fdr = control.fdr, line.thickness = line.thickness,
                     critical.t = critical.t, sig.color = sig.color,
                     insig.color = insig.color, mod.range = mod.range,
                     title = title, y.label = y.label)
  if (!length(out$jn)) stop_wrap("Johnson-Neyman intervals require a numeric moderator.")
  jn <- out$jn[[1]]
  if (!is.null(modx.label)) jn$plot <- jn$plot + ggplot2::labs(x = modx.label)
  attr(jn, "plot") <- plot
  jn
}

#' @export
johnson_neyman.wbgee <- johnson_neyman.wbm

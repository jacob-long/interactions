# Solve the confidence-band crossing polynomial, including linear/constant cases.
contrast_roots <- function(a, b, c) {
  scale <- max(abs(c(a, b, c)))
  if (scale == 0) return(numeric())
  a <- a / scale
  b <- b / scale
  c <- c / scale
  tolerance <- 100 * .Machine$double.eps
  if (abs(a) <= tolerance) {
    return(if (abs(b) <= tolerance) numeric() else -c / b)
  }
  discriminant <- b^2 - 4 * a * c
  disc_tol <- tolerance * max(b^2, abs(4 * a * c))
  if (discriminant < -disc_tol) return(numeric())
  if (abs(discriminant) <= disc_tol) return(-b / (2 * a))
  q <- -.5 * (b + if (b >= 0) sqrt(discriminant) else -sqrt(discriminant))
  sort(c(q / a, c / q))
}

contrast_johnson_neyman <- function(effect, covariance, modrange, pred, modx,
                                     df = Inf, alpha = .05, plot = FALSE, digits = 2,
                                     center = 0, scale = 1,
                                     caption = NULL, control.fdr = FALSE,
                                     line.thickness = .5, critical.t = NULL,
                                     sig.color = "#00BFC4", insig.color = "#F8766D",
                                     mod.range = NULL, title = "Johnson-Neyman plot",
                                     y.label = NULL, modx.label = NULL) {
  if (control.fdr) stop_wrap("FDR-adjusted Johnson-Neyman intervals are not yet supported for panelr models.")
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop_wrap("alpha must be between 0 and 1.")
  }
  critical <- if (is.null(critical.t)) stats::qt(1 - alpha / 2, df) else critical.t
  if (!is.numeric(critical) || length(critical) != 1L || !is.finite(critical) || critical <= 0) {
    stop_wrap("critical.t must be a positive finite number.")
  }
  display <- if (is.null(mod.range)) modrange else mod.range
  if (!is.numeric(display) || length(display) != 2L || any(!is.finite(display)) || display[1] >= display[2]) {
    stop_wrap("The Johnson-Neyman plot needs an increasing finite moderator range.")
  }
  a <- effect[1]
  b <- effect[2]
  V <- covariance
  # A positive value means the squared slope exceeds its squared critical SE.
  polynomial <- c(b^2 - critical^2 * V[2, 2],
                   2 * (a * b - critical^2 * V[1, 2]),
                   a^2 - critical^2 * V[1, 1])
  unit_roots <- contrast_roots(polynomial[1], polynomial[2], polynomial[3])
  roots <- center + scale * unit_roots
  endpoints <- c(-Inf, unit_roots, Inf)
  intervals <- data.frame(lower = utils::head(endpoints, -1), upper = utils::tail(endpoints, -1))
  probe <- vapply(seq_len(nrow(intervals)), function(i) {
    lo <- intervals$lower[i]
    hi <- intervals$upper[i]
    if (is.finite(lo) && is.finite(hi)) return(lo / 2 + hi / 2)
    if (is.finite(lo)) return(lo + max(1, abs(lo)))
    if (is.finite(hi)) return(hi - max(1, abs(hi)))
    0
  }, numeric(1))
  intervals$significant <- (polynomial[1] * probe + polynomial[2]) * probe + polynomial[3] > 0
  intervals$lower <- center + scale * intervals$lower
  intervals$upper <- center + scale * intervals$upper
  moderator <- sort(unique(c(seq(display[1], display[2], length.out = 1000),
                            roots[roots >= display[1] & roots <= display[2]])))
  L <- cbind(1, (moderator - center) / scale)
  slope <- drop(L %*% effect)
  se <- sqrt(pmax(0, rowSums((L %*% V) * L)))
  significant <- abs(slope) > critical * se
  cbs <- data.frame(moderator, slope, Lower = slope - critical * se,
                     Upper = slope + critical * se,
                     Significance = factor(ifelse(significant, "Significant", "Insignificant"),
                                            levels = c("Insignificant", "Significant")))
  cbs$.group <- cumsum(c(TRUE, diff(as.integer(cbs$Significance)) != 0))
  pred_label <- paste("Slope of", pred)
  names(cbs)[1:2] <- c(modx, pred_label)
  p <- ggplot2::ggplot(cbs, ggplot2::aes(x = !!sym(modx), y = !!sym(pred_label),
                     colour = !!sym("Significance"), group = !!sym(".group"))) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = !!sym("Lower"), ymax = !!sym("Upper"),
                         fill = !!sym("Significance")), alpha = .2, colour = NA) +
    ggplot2::geom_line(linewidth = line.thickness) +
    ggplot2::scale_colour_manual(values = c(Insignificant = insig.color, Significant = sig.color), drop = FALSE) +
    ggplot2::scale_fill_manual(values = c(Insignificant = insig.color, Significant = sig.color), drop = FALSE) +
    jtools::theme_nice() +
    ggplot2::labs(title = title, x = if (is.null(modx.label)) modx else modx.label,
                  y = if (is.null(y.label)) pred_label else y.label, caption = caption)
  visible_roots <- roots[roots >= display[1] & roots <= display[2]]
  if (length(visible_roots)) {
    p <- p + ggplot2::geom_vline(xintercept = visible_roots, linetype = 3, colour = "grey50")
  }
  names(roots) <- if (length(roots) == 2L) c("Lower", "Higher") else rep("Boundary", length(roots))
  structure(list(bounds = roots, intervals = intervals, cbands = cbs, plot = p,
                   effect = effect, covariance = covariance, center = center,
                   scale = scale, t_value = critical),
              class = c("johnson_neyman_contrast", "johnson_neyman"),
              pred = pred, modx = modx, modrange = modrange, alpha = alpha,
              plot = plot, digits = digits, control.fdr = FALSE, inference.df = df,
              inside = length(roots) == 2L && intervals$significant[2],
              failed = FALSE, all_sig = all(intervals$significant), caption = caption)
}

#' @export
print.johnson_neyman_contrast <- function(x, ...) {
  cli::cat_line(cli::style_bold("JOHNSON-NEYMAN ANALYSIS"), "\n")
  if (!is.null(attr(x, "caption"))) cat_wrap(attr(x, "caption"), brk = "\n\n")
  significant <- x$intervals[x$intervals$significant, , drop = FALSE]
  if (!nrow(significant)) {
    cat_wrap("The slope does not exclude zero at the selected confidence level anywhere on the moderator scale.", brk = "\n\n")
  } else {
    regions <- apply(significant, 1, function(row) {
      paste0("(", num_print(row[1], attr(x, "digits")), ", ",
               num_print(row[2], attr(x, "digits")), ")")
    })
    cat_wrap("The slope of ", attr(x, "pred"), " excludes zero when ", attr(x, "modx"),
              " is in ", paste(regions, collapse = " or "),
              " (pointwise alpha = ", attr(x, "alpha"), ").", brk = "\n\n")
  }
  observed <- attr(x, "modrange")
  cat_wrap("Observed moderator range: [", num_print(observed[1], attr(x, "digits")),
            ", ", num_print(observed[2], attr(x, "digits")),
            "]. Regions beyond this range extrapolate.", brk = "\n\n")
  if (isTRUE(attr(x, "plot"))) print(x$plot)
  invisible(x)
}

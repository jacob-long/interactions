# Reparameterization must preserve the fitted sample and ancillary quantities.
prepare_lm_slopes_refit <- function(model, data = NULL) {
  frame <- stats::model.frame(model)
  original_formula <- stats::formula(model)
  tt <- stats::terms(model)
  form <- original_formula
  if (length(attr(tt, "offset"))) {
    # Supply the fitted total offset once, including formula and argument offsets.
    rhs <- stats::reformulate(attr(tt, "term.labels"),
                              intercept = attr(tt, "intercept"),
                              env = environment(original_formula))
    form[[3]] <- rhs[[2]]
  }
  required <- all.vars(form)
  if (is.null(data) && all(required %in% names(frame))) {
    d <- frame[required]
  } else {
    if (is.null(data)) {
      data <- tryCatch(eval(getCall(model)$data, environment(original_formula)),
                        error = function(e) NULL)
    }
    if (is.null(data)) {
      stop_wrap("Cannot recover the original variables for simple-slopes refits.",
                  " Supply the original fitting data with data =.")
    }
    data <- as.data.frame(data)
    rows <- match(rownames(frame), rownames(data))
    if (anyNA(rows) || !all(required %in% names(data))) {
      stop_wrap("Simple-slopes data must contain the original fitting variables",
                  " and observation row names, including every fitted observation.")
    }
    d <- data[rows, , drop = FALSE]
    rebuilt <- stats::model.frame(form, d, na.action = stats::na.pass,
                                  xlev = model$xlevels)
    expected <- frame[names(rebuilt)]
    if (!isTRUE(all.equal(rebuilt, expected, check.attributes = FALSE))) {
      stop_wrap("The supplied data do not reproduce the fitted model frame.",
                  " Use unchanged fitting data; unsupported transformations",
                  " may also prevent a valid simple-slopes refit.")
    }
  }

  weights <- stats::model.weights(frame)
  offset <- stats::model.offset(frame)
  wname <- offname <- NULL
  if (!is.null(weights)) {
    wname <- utils::tail(make.unique(c(names(d), ".slopes_weights")), 1L)
    d[[wname]] <- weights
  }
  if (!is.null(offset)) {
    offname <- utils::tail(make.unique(c(names(d), ".slopes_offset")), 1L)
    d[[offname]] <- offset
  }
  refit <- model
  refit$call$formula <- form
  refit$call$subset <- NULL
  refit$call$weights <- if (is.null(wname)) NULL else as.name(wname)
  refit$call$offset <- if (is.null(offname)) NULL else as.name(offname)
  refit$call$na.action <- stats::na.fail
  if (inherits(model, "glm")) refit$call$family <- model$family
  list(model = refit, data = d, weights = weights, wname = wname,
         offname = offname)
}

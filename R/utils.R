## Quicker way to get last item of vector
last <- function(x) {return(x[length(x)])}
## Just so code reads more clearly when using last(x)
first <- function(x) {return(x[1])}

# Get levels if they exist, otherwise unique
ulevels <- function(x) {
  if (!is.null(levels(x))) {
    return(levels(x))
  } else {
    if (!is.numeric(x)) {
      return(unique(x))
    } else {
      return(sort(unique(x)))
    }
  }
}

make_ci_labs <- function(ci.width) {

  alpha <- (1 - ci.width) / 2

  lci_lab <- 0 + alpha
  lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

  uci_lab <- 1 - alpha
  uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

  list(lci = lci_lab, uci = uci_lab)

}


mean_or_base <- function(x, weights = NA) {
  if (is.numeric(x)) {
    if (all(is.na(weights))) {
      mean(x, na.rm = TRUE)
    } else {
      weighted.mean(x, weights, na.rm = TRUE)
    }
  } else if (!is.logical(x)) {
    levels(factor(x))[1]
  } else {
    FALSE
  }
}

## Taken from panelr for handling non-synactic variable names
bt <- function(x) {
  if (!is.null(x)) {
    btv <- paste0("`", x, "`")
    btv <- gsub("``", "`", btv, fixed = TRUE)
    btv <- btv %not% c("", "`")
  } else btv <- NULL
  return(btv)
}

un_bt <- function(x) {
  gsub("`", "", x)
}

# bt_if_needed <- function(string) {
#   if (make.names(string) != string) {
#     return(bt(string))
#   } else {
#     return(string)
#   }
# }

## Taken from Hmisc package to avoid importing for a minor feature
## Added "levels.median"
#' @importFrom stats approx
#'
cut2 <- function(x, cuts, m = 150, g, levels.mean = FALSE,
                 levels.median = FALSE, digits,
                 minmax = TRUE, oneval = TRUE, onlycuts = FALSE) {
  method <- 1
  x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  if (missing(digits))
    digits <- if (levels.mean)
      5
  else 3
  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))
  xlab <- attr(x, "label")
  if (missing(cuts)) {
    nnm <- sum(!is.na(x))
    if (missing(g))
      g <- max(1, floor(nnm/m))
    if (g < 1)
      stop("g must be >=1, m must be positive")
    options(digits = 15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits = digits)
    cum <- cumsum(n)
    m <- length(xx)
    y <- as.integer(ifelse(is.na(x), NA, 1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant",
                   rule = 2, f = 1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e+45
    up <- low <- double(g)
    i <- 0
    for (j in 1:g) {
      cj <- if (method == 1 || j == 1)
        cuts[j]
      else {
        if (i == 0)
          stop("program logic error")
        s <- if (is.na(lower))
          FALSE
        else xx >= lower
        cum.used <- if (all(s))
          0
        else max(cum[!s])
        if (j == m)
          max(xx)
        else if (sum(s) < 2)
          max(xx)
        else approx(cum[s] - cum.used, xx[s], xout = (nnm -
                                                        cum.used)/(g - j + 1),
                    method = "constant",
                    rule = 2, f = 1)$y
      }
      if (cj == upper)
        next
      i <- i + 1
      upper <- cj
      y[x >= (lower - min.dif.factor * min.dif)] <- i
      low[i] <- lower
      lower <- if (j == g)
        upper
      else min(xx[xx > upper])
      if (is.na(lower))
        lower <- upper
      up[i] <- lower
    }
    low <- low[1:i]
    up <- up[1:i]
    variation <- logical(i)
    for (ii in 1:i) {
      r <- range(x[y == ii], na.rm = TRUE)
      variation[ii] <- diff(r) > 0
    }
    if (onlycuts)
      return(unique(c(low, max(xx))))
    flow <- format(low)
    fup <- format(up)
    bb <- c(rep(")", i - 1), "]")
    labs <- ifelse(low == up | (oneval & !variation), flow,
                   paste("[", flow, ",", fup, bb, sep = ""))
    ss <- y == 0 & !is.na(y)
    if (any(ss))
      stop_wrap("categorization error in cut2.  Values of x not appearing in
                any interval:", paste(format(x[ss], digits = 12),
                                      collapse = " "),
                "Lower endpoints:", paste(format(low, digits = 12),
                                          collapse = " "),
                "\nUpper endpoints:", paste(format(up, digits = 12),
                                            collapse = " "))
    y <- structure(y, class = "factor", levels = labs)
  } else {
    if (minmax) {
      r <- range(x, na.rm = TRUE)
      if (r[1] < cuts[1])
        cuts <- c(r[1], cuts)
      if (r[2] > max(cuts))
        cuts <- c(cuts, r[2])
    }
    l <- length(cuts)
    k2 <- cuts - min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    if (!levels.mean) {
      brack <- rep(")", l - 1)
      brack[l - 1] <- "]"
      fmt <- format(cuts)
      labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l],
                    brack, sep = "")
      if (oneval) {
        nu <- table(cut(x.unique, k2))
        if (length(nu) != length(levels(y)))
          stop("program logic error")
        levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)],
                                       fmt[l]), labs)
      }
      else levels(y) <- labs
    }
  }
  if (levels.mean) {
    means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
    levels(y) <- format(means)
  } else if (levels.median) {
    medians <- tapply(x, y, function(w) median(w, na.rm = TRUE))
    levels(y) <- format(medians)
  }
  attr(y, "class") <- "factor"
  # if (length(xlab))
  #   label(y) <- xlab
  y
}

# Some shorthand functions to automatically exclude NA
quant <- function(x, ...) {
  quantile(x, ..., na.rm = TRUE)
}
min2 <- function(...) {
  min(..., na.rm = TRUE)
}
max2 <- function(...) {
  max(..., na.rm = TRUE)
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

#'@export
#'@importFrom generics tidy
generics::tidy

#'@export
#'@importFrom generics glance
generics::glance

### Hadley update #############################################################
# modified from https://stackoverflow.com/questions/13690184/update-inside-a-function-
# only-searches-the-global-environment
#' @importFrom stats update.formula

j_update <- function(mod, formula = NULL, data = NULL, offset = NULL,
                     weights = NULL, call.env = parent.frame(), ...) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }

  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  # Jacob add
  # if (!is.null(offset))
  call$offset <- offset
  # if (!is.null(weights))
  call$weights <- weights


  extras <- as.list(match.call())[-1]
  extras <- extras[which(names(extras) %nin% c("mod", "formula", "data",
                                               "offset", "weights",
                                               "call.env"))]
  for (i in seq_along(extras)) {
    if (is.name(extras[[i]])) {
      extras[[i]] <- eval(extras[[i]], envir = call.env)
    }
  }

  existing <- !is.na(match(names(extras), names(call)))
  for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
  if (any(!existing)) {
    call <- c(as.list(call), extras[!existing])
    call <- as.call(call)
  }

  if (is.null(call.env)) {call.env <- parent.frame()}

  eval(call, env, call.env)
}

# adapted from https://stackoverflow.com/a/42742370
# Looking for whether a method is defined for a given object (...)
# getS3method() doesn't work for something like merMod because the string
# "merMod" is not in the vector returned by class()
#' @importFrom utils methods
check_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  tryCatch({
    X(...)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_margins.R
\name{tidy.sim_margins}
\alias{tidy.sim_margins}
\alias{glance.sim_margins}
\title{Tidiers for \code{\link[=sim_margins]{sim_margins()}} objects.}
\usage{
\method{tidy}{sim_margins}(x, conf.level = 0.95, ...)

\method{glance}{sim_margins}(x, ...)
}
\arguments{
\item{x}{The \code{sim_margins} object}

\item{conf.level}{The width of confidence intervals. Default is .95 (95\%).}

\item{...}{Ignored.}
}
\description{
You can use \code{\link[broom:reexports]{broom::tidy()}} and \code{\link[broom:reexports]{broom::glance()}} for "tidy"
methods of storing \code{sim_margins} output.
}

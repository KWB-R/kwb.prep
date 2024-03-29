% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doRegroupings.R
\name{doRegroupings}
\alias{doRegroupings}
\title{Apply Regrouping of Values in a Data Frame}
\usage{
doRegroupings(
  Data,
  regroup.actual = kwb.utils::selectElements(settings, "regroup.actual"),
  regroup.config = kwb.utils::selectElements(settings, "regroup.config"),
  settings = NULL,
  checkRemaining = TRUE,
  to.factor = FALSE,
  to.numeric = TRUE,
  dbg = TRUE
)
}
\arguments{
\item{Data}{data frame}

\item{regroup.actual}{default: settings$regroup.actual}

\item{regroup.config}{default: settings$regroup.config}

\item{settings}{list of settings that may contain the elements
\code{regroup.actual} and \code{regroup.config}}

\item{checkRemaining}{if TRUE (default) it is checked if all values that
occurred in a column to be regrouped have been considered in the regrouping}

\item{to.factor}{if \code{TRUE} the new values are converted to
\code{factor}. The default is \code{FALSE}.}

\item{to.numeric}{(default: \code{TRUE}, overrides \code{to.factor}!), passed
to \code{\link{regroup}}}

\item{dbg}{if \code{TRUE} (default) debug messages are shown}
}
\description{
Apply Regrouping of Values in a Data Frame
}

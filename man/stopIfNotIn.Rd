% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_berlin.R
\name{stopIfNotIn}
\alias{stopIfNotIn}
\title{Stop with Info If Element Is Not in Expected Set}
\usage{
stopIfNotIn(
  element,
  elements,
  singular = "option",
  plural = paste0(singular, "s"),
  do_stop = TRUE
)
}
\arguments{
\item{element}{element to be looked for in \code{elements}}

\item{elements}{vector of possible elements}

\item{singular}{name of object to appear in error message. Default:
\code{"option"}}

\item{plural}{name of object (plural) to appear in error message. Default:
\code{paste0(singular, "s")}}

\item{do_stop}{if \code{FALSE} (the default is \code{TRUE}) program execution
does not stop. Instead a message is shown.}
}
\description{
Stop with info message if element is not in expected set of elements
}

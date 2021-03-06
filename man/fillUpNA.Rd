% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_berlin.R
\name{fillUpNA}
\alias{fillUpNA}
\title{Fill NA in First Vector With Values From Second Vector}
\usage{
fillUpNA(x, y, dbg = TRUE, name_x = NULL, name_y = NULL)
}
\arguments{
\item{x}{first vector}

\item{y}{second vector}

\item{dbg}{if \code{TRUE} a debug message is shown}

\item{name_x}{name of x}

\item{name_y}{name of y}
}
\value{
\code{x} with \code{NA} replaced by the values in \code{y} at the
same positions
}
\description{
Fill NA in First Vector With Values From Second Vector
}

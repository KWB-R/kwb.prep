% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doRegroupings.R
\name{regroup}
\alias{regroup}
\title{Assign Values to Groups of Values}
\usage{
regroup(x, assignments, ignore.case = NULL, to.factor = FALSE)
}
\arguments{
\item{x}{vector of values}

\item{assignments}{list of assignments of the form \<key\> = \<values\> with
\<values\> being a vector of elements to be looked up in \code{x} and to be
replaced with \<key\> in the output vector}

\item{ignore.case}{if \code{TRUE} the case is ignored when comparing values}

\item{to.factor}{if \code{TRUE} the new values are converted to
\code{factor}. The default is \code{FALSE}.}
}
\value{
vector with as many elements as there are elements in \code{x}. The
vector contains \<key\> at positions where the elements in \code{x} appeared
in the vector \<values\> of a \<key\> = \<values\> assignment of
\code{assignments}
}
\description{
Assign Values to Groups of Values
}
\examples{
regroup(c("A", "B", "C", "D"), assignments = list(
  "AB" = c("A", "B"),
  "CD" = c("C", "D")
))
  
regroup(c("A", "B", "C", "D", "E", "A"), assignments = list(
  "AB" = c("A", "B"),
  "CD" = c("C", "D")
))

}

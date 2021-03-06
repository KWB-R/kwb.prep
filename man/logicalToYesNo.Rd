% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_berlin.R
\name{logicalToYesNo}
\alias{logicalToYesNo}
\title{Convert Vector of Logical to Vector of "Ja"/"Nein"}
\usage{
logicalToYesNo(x, yesno = c("Ja", "Nein"))
}
\arguments{
\item{x}{vector of logical}

\item{yesno}{vector of character of length two giving the strings to be used
for \code{TRUE} and \code{FALSE}, respectively}
}
\value{
vector of character
}
\description{
Convert Vector of Logical to Vector of "Ja"/"Nein"
}
\examples{
logicalToYesNo(c(TRUE, FALSE, TRUE))
logicalToYesNo(c(TRUE, FALSE, TRUE), yesno = c("Yeah!", "Oh no!"))

}

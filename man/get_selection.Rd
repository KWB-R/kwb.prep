% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_selection.R
\name{get_selection}
\alias{get_selection}
\title{Get a Set of Column Names from a Data Frame Defining Selections}
\usage{
get_selection(
  number = 1,
  data = NULL,
  reader = read_csv_file,
  ...,
  column = paste0("select.", number),
  target = "column"
)
}
\arguments{
\item{number}{number of the selection group, default: 1}

\item{data}{data frame defining groups of columns}

\item{reader}{reader function providing \code{data}. Default:
\code{kwb.prep:::read_csv_file}}

\item{\dots}{arguments passed to the \code{reader} function}

\item{column}{name of column in \code{data} containing numbers to
indicate which columns to select in which order}

\item{target}{name of column in \code{data} containing the column
names}
}
\value{
vector of column names
}
\description{
Get a Set of Column Names from a Data Frame Defining Selections
}

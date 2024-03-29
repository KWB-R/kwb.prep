% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_csv_file.R
\name{read_csv_file}
\alias{read_csv_file}
\title{Read Data Frame From CSV File}
\usage{
read_csv_file(
  file,
  sep = get_column_separator(),
  dec = ",",
  encoding = "UTF-8",
  na.strings = "",
  ...,
  remove_comments = TRUE,
  set_empty_string_to_na = FALSE,
  dbg = 1L
)
}
\arguments{
\item{file}{path to csv file}

\item{sep}{Column separator character. Default: semicolon ";"}

\item{dec}{Decimal separator character. Default: comma ","}

\item{encoding}{file encoding string. Default: "UTF-8". Possible other value:
"unknown"}

\item{na.strings}{strings occurring in the files representing NA (not
available). Default: ""}

\item{\dots}{further arguments passed to
\code{\link[data.table]{fread}}}

\item{remove_comments}{Should rows starting with "#" be removed (the
default)?}

\item{set_empty_string_to_na}{if \code{TRUE} (the default is \code{FALSE})
empty strings in character columns are replaced with \code{NA}}

\item{dbg}{if \code{TRUE} debug messages are shown}
}
\description{
Read Data Frame From CSV File
}

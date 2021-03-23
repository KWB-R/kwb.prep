# get_renamings ----------------------------------------------------------------

#' Get List Defining Renamings from Data Frame
#' 
#' Get list defining renamings in the form of \code{from = to} assignments from
#' a data frame read by a function that may be specified.
#' 
#' @param from name of column of \code{data} to take the "from" values
#'   from
#' @param to name of column of \code{data} to take the "to" values from
#' @param data data frame defining renamings
#' @param reader reader function providing \code{data}. Default: 
#'   \code{kwb.prep:::read_csv_file}
#' @param \dots arguments passed to the \code{reader} function
#' @return list defining renamings as e.g. expected by 
#'   \code{\link[kwb.utils]{renameColumns}}
#' 
#' @export
#' 
get_renamings <- function(
  from, to = "column", data = NULL, reader = read_csv_file, ...
)
{
  data <- kwb.utils::defaultIfNULL(data, reader(...))
  
  x <- kwb.utils::selectColumns(data, c(from, to))
  
  x <- x[! kwb.utils::isNaOrEmpty(x[, from]), ]
  
  kwb.utils::toLookupList(keys = x[, from], values = x[, to])
}

# get_selection ----------------------------------------------------------------

#' Get a Set of Column Names from a Data Frame Defining Selections
#' 
#' @param number number of the selection group, default: 1
#' @param data data frame defining groups of columns
#' @param reader reader function providing \code{data}. Default: 
#'   \code{kwb.prep:::read_csv_file}
#' @param \dots arguments passed to the \code{reader} function
#' @param column name of column in \code{data} containing numbers to
#'   indicate which columns to select in which order
#' @param target name of column in \code{data} containing the column
#'   names
#' 
#' @return vector of column names
#' 
#' @export
#' 
get_selection <- function(
  number = 1, 
  data = NULL,
  reader = read_csv_file,
  ...,
  column = paste0("select.", number), 
  target = "column"
)
{
  data <- kwb.utils::defaultIfNULL(data, reader(...))
  
  x <- kwb.utils::selectColumns(data, c(target, column))
  
  x <- x[grep("\\d+", x[[column]]), ]
  
  if (nrow(x) == 0) {
    
    stop_(
      "There are no numbers in column '", column, "' of the selection table. ",
      "The column must be formatted as 'numeric' in Excel!"
    )
  }
  
  orderNumber <- as.integer(x[[column]])
  
  # The ordering numbers should not be duplicated
  duplicates <- orderNumber[duplicated(orderNumber)]
  
  if (length(duplicates) > 0) {
    
    warning(sprintf(
      paste(
        "The values in column '%s' of '%s' should not be duplicated!",
        "Duplicated numbers: %s"
      ),
      column, file, kwb.utils::stringList(duplicates)
    ))
  }
  
  x[order(orderNumber), target]
}

# applyFilterCriteria ----------------------------------------------------------

#' Filter Rows from Data Frame Matching Criteria
#' 
#' Details about criteria applied and number of rows matching each criterion
#' is returned in the attribute "details.filter"
#' 
#' @param x data frame
#' @param criteria vector of character defining filter criteria to be evaluated
#'   in x
#' @param lengthColumn name of the column containing lengths, e.g. "Length_raw"
#' @param \dots passed to \code{\link[kwb.utils]{matchesCriteria}}
#' 
#' @export
#' 
#' @examples 
#' # Create a very simple data frame
#' df <- data.frame(value = 1:10, group = rep(c("a", "b"), 5))
#' 
#' # Show the data frame
#' df
#' 
#' # Filter for rows meeting two criteria
#' result <- applyFilterCriteria(df, c(
#'   "value is below or equal to 5" = "value <= 5", 
#'   "group is 'a'" = "group == 'a'"
#' ))
#' 
#' # Show the result
#' result
#' 
#' # Get the evaluation of each criterion in columns
#' kwb.utils::getAttribute(result, "matches")
#' 
applyFilterCriteria <- function(x, criteria = NULL, lengthColumn = NULL, ...)
{
  if (! is.data.frame(x))
    stop_("x must be a data frame!")
  
  if (is.null(criteria)) {
    
    message("No filter criteria to apply. Returning the data frame unchanged.")
    
    return(x)
  }
  
  matches <- kwb.utils::matchesCriteria(x, criteria, add.details = TRUE, ...)
  
  details <- kwb.utils::getAttribute(matches, "details")
  
  cumMatrix <- cumulateMatchMatrix(details)
  
  x.base <- as.data.frame(details)
  y.base <- as.data.frame(cumMatrix)
  
  if (! is.null(lengthColumn)) {
    lengths <- kwb.utils::selectColumns(x, lengthColumn)
    x.base <- cbind(Length = lengths, x.base)
    y.base <- cbind(Length = lengths, y.base)
  }
  
  outList <- lapply(
    X = seq_len(ncol(details)), 
    FUN = keepOrGoSummary, 
    lengthColumn = lengthColumn, 
    x.base = x.base, 
    y.base = y.base,
    criteriaNames = .getNames(
      criteria, 
      defaultNames = sprintf("Keep '%s'", as.character(criteria))
    )
  )
  
  details.filter <- kwb.utils::rbindAll(outList)
  
  structure(x[matches, ], details.filter = details.filter, matches = matches)
}

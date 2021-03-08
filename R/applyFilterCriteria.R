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
  if (! is.data.frame(x)) {
    stop_("x must be a data frame!")
  }
  
  if (is.null(criteria)) {
    
    message("No filter criteria to apply. Returning the data frame unchanged.")
    
    return(x)
  }
  
  matches <- kwb.utils::matchesCriteria(x, criteria, add.details = TRUE, ...)
  #matches <- kwb.utils::matchesCriteria(x, criteria, add.details = TRUE)
  
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
    criteriaNames = getNames(criteria, defaultNames = sprintf(
      "Keep '%s'", as.character(criteria)
    ))
  )
  
  details.filter <- kwb.utils::rbindAll(outList)
  
  structure(
    x[matches, , drop = FALSE], 
    details.filter = details.filter, 
    matches = matches
  )
}

# cumulateMatchMatrix ----------------------------------------------------------
cumulateMatchMatrix <- function(matchMatrix)
{
  out <- matchMatrix
  
  for (i in seq_len(ncol(out) - 1) + 1) {
    out[, i] <- out[, i-1] & matchMatrix[, i]
  }
  
  out  
}

# keepOrGoSummary --------------------------------------------------------------
keepOrGoSummary <- function(i, lengthColumn, x.base, y.base, criteriaNames)
{
  #i <- 1L
  conditionID <- paste0("C", i)
  
  if (is.null(lengthColumn)) {
    
    lengthColumn2 <- ""
    
  } else {
    
    lengthColumn2 <- "Length"
    
    # aggregate() within fieldSummary() does not work as expected if all Lengths
    # are NA. So we temporarily set NA to 0.0
    get <- kwb.utils::selectColumns
    default <- kwb.utils::defaultIfNA
    x.base$Length <- default(get(x.base, lengthColumn2), 0.0)
    y.base$Length <- default(get(y.base, lengthColumn2), 0.0)
  }
  
  x.out <- fieldSummary(x = x.base, groupBy = conditionID, lengthColumn2)
  y.out <- fieldSummary(x = y.base, groupBy = conditionID, lengthColumn2)
  
  # Select the information on the excluded and of the remaining
  x.out <- x.out[x.out[, conditionID] == FALSE, ]
  y.out <- y.out[y.out[, conditionID] == TRUE,  ]
  
  # If the condition was always met generate a zero-entry
  if (nrow(x.out) == 0) {
    x.out <- rbind(x.out, kwb.utils::toLookupTable(names(x.out), rep(0, ncol(x.out))))
  }
  
  # If the condition was never met generate a zero-entry
  if (nrow(y.out) == 0) {
    y.out <- rbind(y.out, kwb.utils::toLookupTable(names(y.out), rep(0, ncol(y.out))))
  }
  
  names(x.out)[-1] <- paste0(names(x.out)[-1], ".go")
  names(y.out)[-1] <- paste0(names(y.out)[-1], ".keep")
  
  out <- cbind(CleaningStep = criteriaNames[i], x.out, y.out)
  
  kwb.utils::removeColumns(out, conditionID)
}

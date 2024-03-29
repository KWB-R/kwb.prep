# applyFilterCriteria ----------------------------------------------------------

#' Filter Rows from Data Frame Matching Criteria
#' 
#' Details about criteria applied and number of rows matching each criterion
#' is returned in the attribute "details.filter". If a criterion evaluates to 
#' \code{NA}, the corresponding row in the data frame is removed (just as if the
#' criterion evaluated to \code{FALSE}). 
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
    clean_stop("x must be a data frame!")
  }
  
  if (is.null(criteria)) {
    message("No filter criteria to apply. Returning the data frame unchanged.")
    return(x)
  }
  
  matches <- kwb.utils::matchesCriteria(
    Data = x, 
    criteria = criteria, 
    na.to.false = TRUE,
    add.details = TRUE, 
    ...
  )

  details <- kwb.utils::getAttribute(matches, "details")
  
  cumMatrix <- cumulateMatchMatrix(details)
  
  x.base <- as.data.frame(details)
  y.base <- as.data.frame(cumMatrix)
  
  if (! is.null(lengthColumn)) {
    lengths <- kwb.utils::selectColumns(x, lengthColumn)
    x.base <- cbind(Length = lengths, x.base)
    y.base <- cbind(Length = lengths, y.base)
  }
  
  details.filter <- kwb.utils::rbindAll(lapply(
    X = seq_len(ncol(details)), 
    FUN = keepOrGoSummary, 
    lengthColumn = lengthColumn, 
    x.base = x.base, 
    y.base = y.base,
    criteriaNames = getNames(criteria, defaultNames = sprintf(
      "Keep '%s'", as.character(criteria)
    ))
  ))
  
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

  # Helper function to generate zero-entry if condition was always/never met
  add_zero_if_no_rows <- function(x) {
    if (nrow(x) > 0L){
      return(x)
    }
    rbind(x, kwb.utils::toLookupTable(names(x), rep(0L, ncol(x))))
  }
  
  # Helper function to add a suffix to all but the first column name
  add_suffix <- function(x, suffix) {
    names(x)[-1] <- paste0(names(x)[-1], suffix)
    x
  }

  out <- cbind(
    CleaningStep = criteriaNames[i], 
    add_suffix(add_zero_if_no_rows(x.out), ".go"), 
    add_suffix(add_zero_if_no_rows(y.out), ".keep")
  )
  
  kwb.utils::removeColumns(out, conditionID)
}

# fieldSummary -----------------------------------------------------------------

#' Frequency of Value Combinations in Data Frame Columns
#' 
#' @param x data frame
#' @param groupBy vector of character naming the columns (fields) in \code{x}
#'   to be included in the evaluation. Default: names of all columns in \code{x}
#'   except the first one (assuming it could be an ID column).
#' @param lengthColumn optional. Name of column in \code{x} to be summed up
#' @param na optional. Value to be treated as \code{NA}. Default: "Unknown"
#' @export
#' @examples 
#' n <- 1000L
#' sample_replace <- function(x, ...) sample(x, size = n, replace = TRUE, ...)
#' x <- data.frame(
#'   pipe_id = 1:n,
#'   material = sample_replace(c("clay", "concrete", "other")),
#'   age_cat = sample_replace(c("young", "old")),
#'   length = as.integer(rnorm(n, 50)),
#'   stringsAsFactors = FALSE
#' )
#' 
#' fieldSummary(x)
#' fieldSummary(x, "age_cat")
#' fieldSummary(x, "material")
#' fieldSummary(x, "material", lengthColumn = "length")
fieldSummary <- function(
  x, groupBy = names(x)[-1L], lengthColumn = "", na = "Unknown"
)
{
  stopifnot(is.data.frame(x))
  kwb.utils::checkForMissingColumns(x, groupBy)
  
  #column=groupBy[1L];lengthColumn=lengthColumn2;na="Unknown"
  if (nrow(x) == 0L) {
    
    message("The data frame given to fieldSummary() has no rows!")
    
    return(do.call(data.frame, args = c(as.list(x[groupBy]), list(
      Count = integer(), Percentage = numeric()
    ))))
  }
  
  for (column in groupBy) {
    values <- kwb.utils::selectColumns(x, column)
    levels(values) <- c(levels(values), na)
    x[[column]] <- kwb.utils::defaultIfNA(values, na)
  }
  
  y <- stats::aggregate(
    kwb.utils::toFormula("Count", groupBy), 
    data = cbind(x, Count = 1L), 
    FUN = length
  )
  
  if (lengthColumn != "") {
    
    y1 <- stats::aggregate(
      kwb.utils::toFormula(lengthColumn, groupBy), 
      data = x, 
      FUN = sum, 
      na.rm = TRUE
    )
    
    y <- merge(y, y1, by = groupBy)
  }
  
  y$Percentage <- 100 * kwb.utils::selectColumns(y, "Count") / nrow(x)
  
  y
}

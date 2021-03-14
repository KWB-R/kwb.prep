# regrouping_is_used -----------------------------------------------------------

#' Which actual regroupings would be used?
#'
#' Which of the actual regroupings would be used if \code{columns} were
#' available in a data frame
#'
#' @param columns vector of column names for which to check if they are subject
#'   to regrouping
#' @param actuals list of elements \code{from} and \code{to}, as returned by
#'   \code{kwb.prep:::read_actual_regrouping}
#' @return vector of logical as long as \code{actuals}. Attribute \code{column}:
#'   which columns would the data frame have after the regrouping?
regrouping_is_used <- function(columns, actuals)
{
  get <- kwb.utils::selectElements

  is_used <- sapply(actuals, function(actual) {

    found <- get(actual, "from") %in% columns

    if (found) {
      columns <<- unique(c(columns, get(actual, "to")))
    }

    found
  })

  structure(unname(is_used), columns = columns)
}

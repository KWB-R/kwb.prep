# applyFilter (moved from kwb.sema) --------------------------------------------

#' Apply Filter Criteria from List
#' 
#' @param data data frame 
#' @param criteria_list list of (named) vectors of character representing 
#'   filter criteria
#' @param element name of list element to be selected fom \code{criteria_list}
#' @param length_column passed to \code{applyFilterCriteria}
#' 
#' @export
#' 
#' @examples 
#' criteria_list <- list(
#'   apple = c("is red or green" = "colour %in% c('red', 'green')"),
#'   banana = c("is not straight" = "! straight")
#' )
#' 
#' fruit_properties <- data.frame(
#'   colour = c("green", "red", "yellow"),
#'   straight = c(TRUE, TRUE, FALSE)
#'   )
#'   
#' applyFilter(fruit_properties, criteria_list, "apple")
#' applyFilter(fruit_properties, criteria_list, "banana")
#' 
applyFilter <- function(data, criteria_list, element, length_column = NULL)
{
  criteria <- kwb.utils::selectElements(criteria_list, element)
  
  applyFilterCriteria(x = data, criteria, lengthColumn = length_column)
}

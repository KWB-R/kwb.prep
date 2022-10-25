#' Get Expected Table Column Types from Config
#' 
#' What are the expected column types as defined in the configuration?
#' 
#' @param table_name name of table for which to lookup column types in config
#' @param config configuration object (list)
#' @param available optional. Names of available table columns. If given, only
#'   the types of these columns are returned. 
#' @return list with one entry per type. Each element contains the names of the 
#'   colums with the corresponding type.
#' @examples
#' config <- list(animals = list(fields = list(
#'   cats = list(field = "Katzen", type = "integer"),
#'   dogs = list(field = "Hunde", type = "double"),
#'   mice = list(field = "Maeuse", type = "integer")
#' )))
#' 
#' kwb.prep:::get_column_classes2("animals", config)
#' 
get_column_classes2 <- function(table_name, config, available = NULL)
{
  x <- kwb.utils::getListNode(config, paste0(table_name, "/fields"))
  
  # If a vector of available fields is given, reduce x to the available entries
  if (!is.null(available)) {
    x <- x[sapply(x, kwb.utils::getListNode, "field") %in% available]
  }
  
  if (kwb.utils::hasZeroLength(x)) {
    return(NULL)
  }
  
  get <- kwb.utils::selectElements
  split(unname(sapply(x, get, "field")), sapply(x, get, "type"))
}

#' Check for Valid Table Name
#' 
#' Check if the argument can be used as a table name
#' 
#' @param table_name R object to be checked for usage as a table name
#' @export
#' @examples
#' try(check_table_name(c("more", "than", "one", "string")))
#' try(check_table_name("one_is_ok"))
check_table_name <- function(table_name)
{
  if (! (is.character(table_name) && length(table_name) == 1L)) {
    clean_stop("table_name must be a vector of character of length one")
  }
}

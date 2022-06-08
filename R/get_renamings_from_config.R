# get_renamings_from_config ----------------------------------------------------

#' Get List of Renamings from Configuration
#'
#' @param config list with one element per table/csv file
#' @param table_name name of list element within \code{config}
#' @param all if \code{FALSE} only the fields with property "required = TRUE"
#'   are considered
#' @return list with original names as names and internal names as values. The
#'   list can be used in a call to \code{\link[kwb.utils]{renameColumns}}
get_renamings_from_config <- function(config, table_name, all = TRUE)
{
  check_csv_config(config)
  check_table_name(table_name)
  
  field_config <- kwb.utils::getListNode(config, paste0(table_name, "/fields"))
  
  if (kwb.utils::hasZeroLength(field_config)) {
    return(list())
  }
  
  fields <- lapply(field_config, kwb.utils::getListNode, "field")
  
  # Reduce to required fields if not all renamings are requested
  if (! all) {
    fields <- fields[names(which(is_required_field(field_config)))]
  }
  
  kwb.utils::revertListAssignments(fields)
}

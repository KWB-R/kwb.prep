# read_actual_regrouping -------------------------------------------------------

#' Read and Filter "regroup_actual.csv"
#' 
#' @param name_actual Base name of file in config folder, default: 
#'   "regroup_actual". The file specifies: which regroupings are arcually to be 
#'   applied? What are the names of input and output columns? 
#' @param group Name of column in \code{<name_actual>.csv} containing non-empty
#'   fields for rows that are to be considered. \code{NULL} (the default) means
#'   that all rows of the file are considered.
#' @param columns names of (input) columns that are to be regrouped. Only those
#'   regroupings are performed that work on these columns or columns that are
#'   created during the regrouping. By default \code{columns = NULL} all
#'   regroupings are used (unless \code{group} is given).
#' @param as_list it \code{TRUE} (the default) the actual regrouping 
#'   configuration is returned as a list (as required by \code{doRegroupings}),
#'   otherwise as a data frame.
read_actual_regrouping <- function(
  name_actual,   
  group = NULL,
  columns = NULL,
  as_list = TRUE
)
{
  #kwb.prep::assign_objects()
  #name_actual="regroup_actual";group=NULL;columns=NULL;as_list=TRUE
  
  if (! is.null(group) && ! is.null(columns)) {
    stop_(
      "Please set either 'group' or 'columns' but not both in ", 
      "read_actual_regrouping()"
    )
  }
  
  actuals_df <- utils::read.table(
    file = config_file(paste0(name_actual, ".csv")),
    sep = ";",
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Filter regroupings either for concerned columns or for a group column
  if (! is.null(columns)) {
    
    actuals_list <- actual_regrouping_to_list(actuals_df)
    actuals_list <- actuals_list[regrouping_is_used(columns, actuals_list)]
    actuals_df <- actual_regrouping_to_df(actuals_list)
    
  } else if (! is.null(group)) {
    
    actuals_df <- filter_actual_regrouping_for_group(actuals_df, group)
  }
  
  if (! as_list) {
    return(actuals_df)
  }

  actual_regrouping_to_list(actuals_df)
}

# actual_regrouping_to_list ----------------------------------------------------
actual_regrouping_to_list <- function(actuals_df)
{
  stats::setNames(
    lapply(seq_len(nrow(actuals_df)), FUN = function(i) {
      as.list(actuals_df[i, actual_regrouping_keys()])
    }),
    kwb.utils::selectColumns(actuals_df, "to")
  )
}

# actual_regrouping_keys -------------------------------------------------------
actual_regrouping_keys <- function()
{
  c("from", "to", "name", "labels")  
}

# actual_regrouping_to_df ------------------------------------------------------
actual_regrouping_to_df <- function(actuals_list)
{
  do.call(rbind, lapply(actuals_list, as.data.frame, stringsAsFactors = FALSE))
}

# filter__actual_regrouping_for_group ------------------------------------------
filter_actual_regrouping_for_group <- function(x, group = NULL)
{
  stopifnot(is.data.frame(x))
  
  if (is.null(group)) {
    return(x)
  }
  
  # Select main columns and group column
  x <- kwb.utils::selectColumns(x, c(actual_regrouping_keys(), group))
  
  # Exclude rows not  belonging to the group
  x <- x[! is.na(kwb.utils::selectColumns(x, group)), ]
  
  # Order by the group column
  x %>%
    kwb.utils::orderBy(group) %>%
    kwb.utils::removeColumns(group) %>%
    kwb.utils::resetRowNames()
}

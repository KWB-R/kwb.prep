# write_filter_info ------------------------------------------------------------

#' Write Information on Filtering to CSV files
#' 
#' @param x data frame as returned by \code{\link{apply_filters}}, with 
#'   attribute \code{filter_info} set.
#' @param target_dir path to directory into which to write csv files
#' @param prefix string by which to prefix all files
#' @return \code{x}, unchanged, invisibly
#' @export
#' @importFrom kwb.utils getAttribute
#' 
write_filter_info <- function(x, target_dir, prefix = deparse(substitute(x)))
{
  stopifnot(is.data.frame(x))
  
  filter_info <- kwb.utils::getAttribute(x, "filter_info")

  # Modify the overview table for each entry in "filter_info"  
  filter_info <- lapply(filter_info, function(x) {
    x$overview <- format_overview_table(x$overview)
    x
  })
  
  dfs <- flatten_data_frame_lists(filter_info,  prefix = prefix)

  # Write csv files into (newly created) sub-folder <target_dir>/<prefix>
  write_data_frames(
    dfs = stats::setNames(dfs, remove_prefix(names(dfs), prefix)), 
    path = kwb.utils::createDirectory(file.path(target_dir, prefix))
  )
  
  invisible(x)
}

# format_overview_table --------------------------------------------------------
format_overview_table <- function(data)
{
  data <- kwb.utils::renameColumns(data, list(CleaningStep = "Step.name"))
  
  STEP_NO <- "Step.no"
  
  data[[STEP_NO]] <- seq_len(nrow(data))
  
  kwb.utils::moveColumnsToFront(data, STEP_NO)
}

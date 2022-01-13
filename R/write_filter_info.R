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
  
  dfs <- flatten_data_frame_lists(filter_info,  prefix = prefix)

  # Write csv files into (newly created) sub-folder <target_dir>/<prefix>
  write_data_frames(
    dfs = stats::setNames(dfs, remove_prefix(names(dfs), prefix)), 
    path = kwb.utils::createDirectory(file.path(target_dir, prefix))
  )
  
  invisible(x)
}

#' List Files in Zip Archive
#'
#' @param zip_file path to zip archive
#' @param include_dirs if \code{TRUE} directory paths are also returned. The
#'   default is \code{FALSE}, i.e. only the paths to files are returned.
#' @return paths to files contained in zip archive
#' @export
get_zipped_paths <- function(zip_file, include_dirs = FALSE)
{
  check_zip_extension(zip_file)
  
  zip_file <- kwb.utils::safePath(path.expand(zip_file))
  zip_info <- archive::archive(zip_file)
  
  if (nrow(zip_info) == 0L) {
    warning(sprintf(
      "archive::archive('%s') returned an empty data frame.", zip_file
    ))
  }
  
  paths <- kwb.utils::selectColumns(zip_info, "path")
  
  if (include_dirs) {
    return(paths)
  }
  
  # Exclude directory paths
  paths[! grepl("/$", paths)]
}

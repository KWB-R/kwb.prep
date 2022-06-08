#' Unzip Archive
#' 
#' @param zip_file path to archive file
#' @param target_dir path to target directory
#' @param flatten if \code{TRUE} (the default) all files in the archive are
#'   unzipped directly into the target directory, independent from possible
#'   folder structures within the archive
#' @param dbg whether or not to show debug messages
unzip_archive <- function(
  zip_file, target_dir = tempdir(), flatten = TRUE, dbg = TRUE
)
{
  # Create temporary directory within the target directory
  folder_name <- kwb.utils::hsSafeName("__import__", tolower(dir(target_dir)))
  tmp_dir <- target_dir %>%
    file.path(folder_name) %>%
    kwb.utils::createDirectory(dbg = FALSE)
  
  # Extract all files to the temporary directory
  kwb.utils::catAndRun(
    dbg = dbg,
    messageText = get_text(
      "unzipping", 
      kwb.utils::fullWinPath(zip_file), 
      kwb.utils::fullWinPath(tmp_dir)
    ), 
    expr = {
      archive::archive_extract(zip_file, dir = tmp_dir)
    })
  
  # Get the paths to all extracted files
  paths <- dir(tmp_dir, full.names = TRUE, recursive = TRUE)
  
  # There must not be duplicated file names!
  check_duplicates(basename(paths))
  
  # Move all files to the target directory, removing the tree structure
  fs::file_move(paths, target_dir)
  
  # Remove the temporary directory
  fs::dir_delete(tmp_dir)
  
  # Return the path to the target directory, invisibly
  invisible(target_dir)
}

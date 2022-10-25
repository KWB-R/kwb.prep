#' Path to (Invalid) Example Zip File
#' 
#' @param \dots (segments of) relative path to the wanted zip file, starting 
#'   from "extdata/testdata/invalid-zips" in the package installation folder
#' @export
#' @examples 
#' # list available files
#' (available <- invalid_zip())
#' # Get full path to the first file
#' invalid_zip(available[1])
invalid_zip <- function(...)
{
  path <- "testdata/invalid-zips"

  # List the files if no arguments are given
  if (length(list(...)) == 0L) {
    return(dir(package_file(path)))
  } 
  
  package_file(path, ...)
}

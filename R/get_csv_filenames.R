#' Get Names of CSV Files Referenced in Config
#' 
#' @param config configuration object (list) with one entry per "table", each
#'   of which is expected to have an entry "file"
#' @param keep_empty logical. Whether or not to keep "file" entries that are 
#'   empty ("")
#' @export
#' @return vector of character with the file names referenced in \code{config}
#' @examples
#' config <- list(
#'   table_a = list(file = "table-a.csv"),
#'   table_b = list(file = "table-b.csv")
#' )
#' get_csv_filenames(config)
get_csv_filenames <- function(config, keep_empty = FALSE)
{
  files <- collect(config, "file", default = "")
  
  if (keep_empty) {
    return(files)
  }
  
  files[files != ""]
}

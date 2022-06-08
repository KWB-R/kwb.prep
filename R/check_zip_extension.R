#' Stop if File Name Does not End with Zip Extension
#' 
#' @param file path to file to check for .zip or .7z file name extension
#' @export
#' @return The function does not return anything but stops with a clear error
#'   message in case that \code{file} does not end with something that looks
#'   like the file extension of a compressed file.
check_zip_extension <- function(file)
{
  if (! has_zip_extension(file)) {
    stop_text("zip_expected", basename(file))
  }
}

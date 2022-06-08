#' Does a File have a Zip Extension (.zip, .7z)?
#' 
#' @param file path(s) to file(s) to be checked for zip extension
#' @param expected expected file name extensions. Default: \code{c("zip", "7z")}
#' @return vector of logical
#' @export
#' @examples
#' all(has_zip_extension(c("a.zip", "b.ZIP", "c.Zip", "d.7z", "e.7Z"))) # TRUE
#' has_zip_extension("a.txt") # FALSE
has_zip_extension <- function(file, expected = c("zip", "7z"))
{
  get_lower_extension(file) %in% expected
}

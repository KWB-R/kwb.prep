# config_file ------------------------------------------------------------------
#' @importFrom kwb.utils createDirectory safePath
#' @keywords internal
config_file <- function(..., must_exist = TRUE)
{
  #system.file("extdata", "config", ..., package = "kwb.prep")
  root <- kwb.utils::createDirectory(path.expand("~/tmp/kwb.prep/config"))

  if (must_exist) {
    kwb.utils::safePath(root, ...)
  } else {
    file.path(root, ...)
  } 
}

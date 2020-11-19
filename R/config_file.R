# config_file ------------------------------------------------------------------
#' @importFrom kwb.utils createDirectory safePath
#' @keywords internal
config_file <- function(..., must_exist = TRUE, in_package = FALSE)
{
  root <- if (in_package) {
    system.file("extdata", "config", package = "kwb.prep")
  } else {
    kwb.utils::createDirectory(dbg = FALSE, path.expand("~/tmp/kwb.prep/config"))
  }
  
  if (must_exist) {
    kwb.utils::safePath(root, ...)
  } else {
    file.path(root, ...)
  } 
}

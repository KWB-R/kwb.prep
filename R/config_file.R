# config_file ------------------------------------------------------------------
#' @importFrom kwb.utils createDirectory safePath
#' @keywords internal
config_file <- function(..., must_exist = TRUE, in_package = FALSE)
{
  root <- if (in_package) {
    
    system.file("extdata", "config", package = "kwb.prep")
    
  } else {
    
    default_path <- path.expand("~/tmp/kwb.prep/config")
    
    get_user_config_dir(default_path)
  }
  
  if (must_exist) {
    kwb.utils::safePath(root, ...)
  } else {
    file.path(root, ...)
  } 
}

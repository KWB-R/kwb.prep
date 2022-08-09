#' Set the Path to the Config Folder
#' 
#' Many functions in this package require configuration files. The idea behind 
#' this is to separate (configurational) data from code. The code can the be
#' the same and only the configuration files need to be exchanged. With this 
#' function you tell kwb.prep where to look for configuration files.
#' 
#' @param path path to config folder containing configuration files
#' @export
set_user_config_dir <- function(path)
{
  set_global("user_config_dir", kwb.utils::safePath(path))
}

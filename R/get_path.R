#' Resolve Path from Path Dictionary in Config Folder
#' 
#' @param x key to be looked up in the path dictionary
#' @param \dots possible key = value assignments to be used to replace 
#'   \<placeholders\> in the path that was looked up
#' @export
get_path <- function(x = NULL, ...)
{
  dictionary <- read_yaml_file(dbg = FALSE, extdata_file(
    "config/path-dictionary.yml"
  ))
  
  if (is.null(x)) {
    return(dictionary)
  }
  
  if (length(x) > 1L) {
    return(unlist(lapply(x, get_path, ...)))
  }
  
  assignments <- list(...)
  
  #assignments <- list(run_dir = "my_run_dir")
  #assignments <- list()
  #x <- "input_dir"
  
  paths <- kwb.utils::callWith(kwb.utils::resolve, x = dictionary, assignments)
  
  kwb.utils::selectElements(paths, x)
}

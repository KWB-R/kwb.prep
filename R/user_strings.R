# get_user_strings -------------------------------------------------------------

#' Get List of User-Defined Text Constants
#' 
#' @export
get_user_strings <- function()
{
  get_global("user_strings")
}

# set_user_strings -------------------------------------------------------------

#' Get List of User-Defined Text Constants
#' 
#' @param x list of key = (character) value assignments
#' @export
#' @importFrom methods allNames
set_user_strings <- function(x)
{
  set_global("user_strings", check_user_strings(x))
}

# check_user_strings -----------------------------------------------------------
check_user_strings <- function(x)
{
  stopifnot(is.list(x))
  stopifnot(all(lengths(x) == 1L))
  stopifnot(all(sapply(x, mode) == "character"))
  stopifnot(all(methods::allNames(x) != ""))
  
  #kwb.prep::assign_objects()
  if (length(common <- intersect(names(x), names(read_string_definition())))) {
    
    stop_(
      "The following string constant keys are not allowed. ", 
      "They are for internal use only:\n- ", 
      kwb.utils::stringList(common, collapse = "\n- ")
    )
  }
  
  invisible(x)
}

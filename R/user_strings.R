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
set_user_strings <- function(x)
{
  stopifnot(is.list(x))
  stopifnot(all(lengths(x) == 1L))
  stopifnot(all(sapply(x, mode) == "character"))
  
  set_global("user_strings", x)
}

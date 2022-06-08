#' Collect Elements of Sublists
#' 
#' @param x a list of lists
#' @param element name of list element to be collected from each sublist of 
#'   \code{x}
#' @param default value to be returned for lists that do not have an element
#'   called \code{element}.
#' @export
#' @examples
#' x <- list(
#'   list(a = 1, b = 2),
#'   list(c = 3, a = 4),
#'   list(d = 5, e = 6)
#' )
#' 
#' collect(x, "a")
#' collect(x, "a", default = 99)
#' 
collect <- function(x, element, default = NULL)
{
  if (is.null(default)) {
    return(unlist(lapply(x, "[[", element)))
  }
  
  sapply(x, function(x) kwb.utils::defaultIfNULL(x[[element]], default))
}

#' Stop with Error Message Looked Up by Keyword
#' 
#' @param \dots arguments passed to \code{\link{get_text}}
#' @export
stop_text <- function(...)
{
  clean_stop(get_text(...))
}

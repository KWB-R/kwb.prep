# kable_translated -------------------------------------------------------------

#' Rename Data Frame Columns and Print as Markdown 
#' 
#' @param x x
#' @param \dots passed to \code{translate_columns}
#' @export
kable_translated <- function(x, ...)
{
  kable_no_rows(translate_columns(x), ...)
}

# translate_columns ------------------------------------------------------------
translate_columns <- function(x)
{
  kwb.utils::renameColumns(x, get_text(names(x)))
}

# kable_no_rows ----------------------------------------------------------------

#' Print Data Frame as Markdown Table (Without Row Names by Default)
#' 
#' @param \dots passed to \code{\link[knitr]{kable}}
#' @param row.names passed to \code{\link[knitr]{kable}}, default: \code{FALSE}
#' @importFrom knitr kable
#' @export
kable_no_rows <- function(..., row.names = FALSE)
{
  knitr::kable(..., row.names = row.names)
}


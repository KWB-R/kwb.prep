# kable_translated -------------------------------------------------------------
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
kable_no_rows <- function(..., row.names = FALSE)
{
  knitr::kable(..., row.names = row.names)
}


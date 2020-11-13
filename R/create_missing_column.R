# create_missing_column --------------------------------------------------------
# TODO: Merge with kwb.utils::hsAddMissingCols() and use that one
create_missing_column <- function(df, column, value = NA)
{
  stopifnot(is.data.frame(df), is.character(column), length(column) == 1L)
  
  if (column %in% colnames(df)) {
    return(df)
  }
  
  kwb.utils::catAndRun(
    get_text("creating_missing_column", column),
    df[[column]] <- value
  )
  
  df
}


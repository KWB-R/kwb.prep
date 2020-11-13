# unique_rows ------------------------------------------------------------------
#' @keywords internal
unique_rows <- function(x, dbg = 2L)
{
  #kwb.prep::assign_objects()
  #x = iris; dbg = 2L
  stopifnot(is.data.frame(x))
  
  before <- nrow(x)
  x <- unique(x)
  after <- nrow(x)
  
  if (dbg) {
    
    removed <- before - after
    
    metadata <- kwb.utils::noFactorDataFrame(
      rows_before = before,
      rows_after = after,
      rows_removed = removed,
      rows_removed_percent = round(kwb.utils::percentage(removed, before), 1L),
      key_columns = list_with_comma(names(x))
    )
    
    metadata %>%
      kable_translated() %>%
      write_markdown_chapter("unique_rows", level = dbg)
  }
  
  x
}

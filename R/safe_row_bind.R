# safe_row_bind ----------------------------------------------------------------
#' @keywords internal
safe_row_bind <- function(x, y, name_x = NULL, name_y = NULL, dbg = 1L)
{
  if (dbg) {
    
    name_x <- getname(name_x, substitute(x))
    name_y <- getname(name_y, substitute(y))
    
    metadata <- kwb.utils::noFactorDataFrame(
      table_name = c(name_x, name_y),
      n_rows = c(nrow(x), nrow(y)),
      n_cols = c(ncol(x), ncol(y))
    )
    
    write_markdown_chapter(
      kable_translated(metadata),
      caption_key = "row_bind",
      level = dbg
    )
  }
  
  kwb.utils::safeRowBind(x, y)
}

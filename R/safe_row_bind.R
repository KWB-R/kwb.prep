# safe_row_bind ----------------------------------------------------------------
#' @keywords internal
safe_row_bind <- function(
  x, y,
  name_x = deparse(substitute(x)),
  name_y = deparse(substitute(y)),
  dbg = 3L
)
{
  if (dbg) {
    
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

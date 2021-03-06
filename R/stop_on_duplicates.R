# stop_on_duplicates -----------------------------------------------------------
stop_on_duplicates <- function(data, columns, dbg = 3L)
{
  stop("stop_on_duplicates() needs to be reimplemented!")
  
  if (dbg) {
    
    write_markdown_chapter(
      get_text("no_duplicates", list_with_comma(columns)),
      caption_key = "duplicate_check",
      level = dbg
    )
  }
  
  invisible(data)
}

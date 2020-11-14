# rename_columns ---------------------------------------------------------------
rename_columns <- function(x, renamings = NULL, dbg = 3L, name = NULL)
{
  name <- getname(name, substitute(x))
  
  before <- names(x)
  
  x <- kwb.utils::renameColumns(x, renamings)
  
  if (dbg && any(differs <- before != (after <- names(x)))) {
    
    write_markdown_chapter(
      knitr::kable(cbind(von = before[differs], nach = after[differs])),
      caption = get_text("renaming_columns", newline_collapsed(name)),
      level = dbg
    )
  }
  
  x
}

# set_columns ------------------------------------------------------------------
set_columns <- function(x, ..., dbg = 1L, name = NULL)
{
  if (dbg) {
    
    name <- getname(name, substitute(x))
    
    write_markdown_chapter(
      to_markdown_enum(names(list(...))),
      caption = if (name == ".") {
        get_text("calculating_new_columns")
      } else {
        get_text("calculating_new_columns_in", name)
      },
      level = dbg
    )
  }
  
  kwb.utils::setColumns(x, ..., dbg = FALSE)
}

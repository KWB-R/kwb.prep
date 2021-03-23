# set_columns ------------------------------------------------------------------
set_columns <- function(x, ..., dbg = 1L, name = NULL)
{
  result <- kwb.utils::setColumns(x, ..., dbg = FALSE)
  
  if (! dbg) {
    return(result)
  }
  
  name <- getname(name, substitute(x))
  
  assignments <- list(...)
  
  descriptions <- sapply(assignments, function(xx) {
    kwb.utils::defaultIfNULL(attr(xx, "description"), "")
  })
  
  items <- names(assignments)
  
  is_given <- descriptions != ""
  
  items[is_given] <- paste0(
    items[is_given], " (", descriptions[is_given], ")"
  )
  
  write_markdown_chapter(
    to_markdown_enum(items),
    caption = if (name == ".") {
      get_text("calculating_new_columns")
    } else {
      get_text("calculating_new_columns_in", name)
    },
    level = dbg
  )
  
  result
}

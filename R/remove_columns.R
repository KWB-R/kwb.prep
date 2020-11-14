# remove_columns ---------------------------------------------------------------
remove_columns <- function(
  x, columns = NULL, reason = NULL, ..., dbg. = TRUE, key = NULL, 
  name = NULL
)
{
  name <- getname(name, substitute(x))
  
  if (! is.null(key)) {
  
    config <- kwb.utils::selectElements(
      read_args("remove_columns", dbg = FALSE), key
    )
    
    return(remove_columns(
      x,
      columns = config$columns,
      reason = config$reason,
      pattern = config$pattern,
      dbg. = dbg.,
      name = name
    ))
  }
  
  before <- names(x)
  
  x <- kwb.utils::removeColumns(x, columns, ..., dbg = FALSE)
  
  removed <- setdiff(before, after <- names(x))
  
  if (! dbg. || ! length(removed) && ! in_development_mode()) {
    return(x)
  }
  
  content <- if (n_removed <- length(removed)) {
    
    md_enum <- to_markdown_enum(removed, collapse = TRUE)
    
    if (is.null(reason)) {
      get_text("columns_removed", n_removed, name, md_enum)
    } else {
      get_text(
        "columns_removed_reason", n_removed, name, get_text(reason), md_enum
      )
    }
    
  } else {
    
    get_text("no_columns_removed")
  }
  
  write_markdown_chapter(
    content, level = dbg., caption = get_text(
      "removing_columns", newline_collapsed(name)
    )
  )
  
  x
}

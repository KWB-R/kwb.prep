# md_header --------------------------------------------------------------------

#' Print Markdown Section Header
#' 
#' @param level level
#' @param caption_key caption_key
#' @param caption caption
#' @param print print
#' @param msg msg
#' @export
#' @importFrom kwb.utils defaultIfNULL repeated
md_header <- function(
  level, caption_key = "key?", caption = NULL, print = TRUE, msg = TRUE
)
{
  #kwb.prep::assign_objects()
  #caption=NULL;print=TRUE;msg=TRUE
  caption <- kwb.utils::defaultIfNULL(caption, get_text(caption_key))
  
  if (level == 0L) {
    return(NULL)
  }
  
  raw_header <- paste(kwb.utils::repeated("#", level), caption)
  
  header <- paste0(get_text("new_line"), raw_header)
  
  message_if(msg, raw_header)
  cat_if(print, header, get_text("new_line"), get_text("new_line"))
  
  # For convenience: return the next debug level to allow for:
  # dbg <- md_header(dbg, ...)
  if (print) {
    invisible(level + 1L)
  } else {
    invisible(header)
  }
}

# print_kable ------------------------------------------------------------------
print_kable <- function(...)
{
  print(knitr::kable(...))
}

# to_markdown_chapter ----------------------------------------------------------
to_markdown_chapter <- function(
  x, caption_key = "key?", level = 3L, caption = NULL
)
{
  c(
    md_header(level, caption_key, caption = caption, print = FALSE),
    "",
    x,
    ""
  )
}

# to_markdown_enum -------------------------------------------------------------
to_markdown_enum <- function(x, collapse = FALSE)
{
  nl <- get_text("new_line")
  
  md <- paste0("* ", x, collapse = nl)
  
  if (! collapse) {
    return(md)
  }
  
  paste(md, collapse = nl)
}

# write_enum -------------------------------------------------------------------
write_enum <- function(x, ...)
{
  writeLines(to_markdown_enum(get_text(x, ...)))
}

# write_enum_if ----------------------------------------------------------------
write_enum_if <- function(check, x, ...)
{
  if (check) {
    write_enum(x, ...)
  }
}

# write_markdown_chapter -------------------------------------------------------

#' Write a Markdown Chapter
#' 
#' @param x x
#' @param caption_key caption_key
#' @param level level
#' @param caption caption
#' @export
write_markdown_chapter <- function(
  x, caption_key = "key?", level = 3L, caption = NULL
)
{
  if (level <= 0L) {
    return()
  }
  
  writeLines(to_markdown_chapter(
    x = x, caption_key = caption_key, level = level, caption = caption
  ))
}

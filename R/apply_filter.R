# apply_filter -----------------------------------------------------------------
apply_filter <- function(
  x, element, length_column = NULL, dbg = 2L, name = NULL, config = NULL
)
{
  name <- getname(name, substitute(x))
 
  if (is.null(config)) {
    config <- read_filter_criteria(dbg = FALSE)
  }
  
  output <- utils::capture.output(
    result <- applyFilter(x, config, element, length_column)
  )
  
  write_markdown_chapter(
    to_rcode_snippet(output),
    level = dbg,
    caption = if (name == ".") {
      get_text("applying_filter", element)
    } else {
      get_text("applying_filter_to", element, name)
    }
  )
  
  details <- attr(x, "details.filter")

  if (! is.null(length_column) && ! is.null(details)) {
    
    writeLines(printFilterTable(details))
    
  } else {
    
    writeLines("Es waren keine Filterkriterien definiert.")
  }
  
  result
}

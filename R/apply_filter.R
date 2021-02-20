# apply_filter -----------------------------------------------------------------
apply_filter <- function(
  x, element, length_column = NULL, dbg = 2L, name = NULL, criteria = NULL
)
{
  #kwb.utils::assignPackageObjects("kwb.prep")
  name <- getname(name, substitute(x))
 
  if (is.null(criteria)) {
    criteria <- read_filter_criteria(dbg = FALSE)
  }
  
  output <- utils::capture.output(
    result <- applyFilter(x, criteria, element, length_column)
  )
  
  write_markdown_chapter(
    if (length(output)) to_rcode_snippet(output) else "",
    level = dbg,
    caption = if (name == ".") {
      get_text("applying_filter", element)
    } else {
      get_text("applying_filter_to", element, name)
    }
  )
  
  details <- attr(result, "details.filter")

  if (is.null(details) || is.null(length_column)) {
    
    text <- paste0(
      "Eine Uebersicht ueber die entfernten Haltungen und deren Laengen kann", 
      "nicht erzeugt werden, da\n\n"
    )
    
    if (is.null(details)) {
      text <- paste0(text, "- ", "keine Filterkriterien definiert waren\n")
    }
    
    if (is.null(length_column)) {
      text <- paste0(text, "- ", "keine Laengenangaben uebergeben wurden\n")
    }
    
    writeLines(text)
    
  } else {
    
    writeLines(printFilterTable(details))
  }
  
  result
}

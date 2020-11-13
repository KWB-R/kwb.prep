# safe_merge -------------------------------------------------------------------
safe_merge <- function(
  x, y, 
  by = intersect(names(x), names(y)), 
  by.x = by, 
  by.y = by, 
  ..., 
  dbg = 1L, 
  name_x = deparse(substitute(x)),
  name_y = deparse(substitute(y))
)
{
  checkForMissingColumns(x, by.x)
  checkForMissingColumns(y, by.y)
  
  #x <- iris;y <- iris[c(2, 4)]
  #kwb.utils::assignArgumentDefaults(kwb.prep:::safe_merge)
  #kwb.prep::assign_objects()
  if (dbg) {
    
    names_x <- setdiff(names(x), by.x)
    names_y <- setdiff(names(y), by.y)
    
    all_names <- c(names_x, setdiff(names_y, names_x))
    
    in_x <- all_names %in% names_x
    in_y <- all_names %in% names_y
    
    types <- function(x) unname(sapply(x, main_class))
    type_if <- function(x, check) ifelse(check, types(x), "")
    col_names <- function(by.i, in_i) c(by.i, ifelse(in_i, all_names, ""))
    type_names <- function(xx, by.i, in_i) c(types(xx[by.i]), type_if(x, in_i))
    
    metadata <- kwb.utils::noFactorDataFrame(
      type_x = type_names(x, by.x, in_x),
      column_x = col_names(by.x, in_x),
      by = c(rep("x", length(by.x)), rep("", length(all_names))),
      column_y = col_names(by.y, in_y),
      type_y = type_names(y, by.y, in_y)
    )
    
    metadata %>%
      kable_translated() %>%
      write_markdown_chapter(
        caption = get_text("merging", name_x, name_y), 
        level = dbg
      )
  }
  
  result <- merge(
    x, y, 
    by = by, 
    by.x = by.x, 
    by.y = by.y, 
    ...
  )
  
  structure(result, metadata = if (dbg) metadata)
}

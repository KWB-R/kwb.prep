# safe_merge -------------------------------------------------------------------
safe_merge <- function(
  x, y, 
  by = intersect(names(x), names(y)), 
  by.x = by, 
  by.y = by, 
  ..., 
  dbg = 1L, 
  name_x = NULL,
  name_y = NULL,
  metadata = dbg > 0L
)
{
  checkForMissingColumns(x, by.x)
  checkForMissingColumns(y, by.y)
  
  #x <- iris;y <- iris[c(2, 4)]
  #kwb.utils::assignArgumentDefaults(kwb.prep:::safe_merge)
  #kwb.prep::assign_objects()
  meta <- safe_merge_metadata(
    x, y, by.x, by.y,
    name_x = getname(name_x, substitute(x)),
    name_y = getname(name_y, substitute(y))
  ) 
  
  if (dbg) {

    meta %>%
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
  
  structure(result, metadata = if (metadata) meta)
}

# safe_merge_metadata ----------------------------------------------------------
safe_merge_metadata <- function(x, y, by.x, by.y, name_x, name_y)
{
  names_x <- setdiff(names(x), by.x)
  names_y <- setdiff(names(y), by.y)
  
  all_names <- c(names_x, setdiff(names_y, names_x))
  
  in.x <- all_names %in% names_x
  in.y <- all_names %in% names_y
  
  types <- function(d) unname(sapply(d, kwb.utils::mainClass))
  type_if <- function(d, check) ifelse(check, types(d), "")
  col_names <- function(b, i) c(b, ifelse(i, all_names, ""))
  type_names <- function(d, b, i) c(types(d[b]), type_if(x, i))
  
  kwb.utils::noFactorDataFrame(
    type_x = type_names(x, by.x, in.x),
    column_x = col_names(by.x, in.x),
    by = c(rep("x", length(by.x)), rep("", length(all_names))),
    column_y = col_names(by.y, in.y),
    type_y = type_names(y, by.y, in.y)
  )
}

# left_join --------------------------------------------------------------------
left_join <- function(
  x, y, by, use_dplyr = TRUE, message_anyway = TRUE, check = TRUE, 
  dbg = get_dbg(), 
  name_x = deparse(substitute(x)), 
  name_y = deparse(substitute(y)),
  name = paste0(name_x, "_", name_y)
)
{
  non_by <- function(df) setdiff(names(df), by)
  
  if (length(common <- intersect(non_by(x), non_by(y)))) {
    message_text(
      "common_columns", name_x, name_y,
      kwb.utils::stringList(common, collapse = "\n- ")
    )
  }
  
  result <- if (use_dplyr) {
    
    dplyr::left_join(x, y, by = by)
    
  } else {
    
    merge(x, y, by = by, all.x = TRUE)
  }
  
  # Check (if requested): The number of rows must not change!
  n_x <- nrow(x)
  n_y <- nrow(y)
  n_result <- nrow(result)
  
  if (check && n_result != n_x) {
    stop_(get_text("merging_failed", name_x, name_y, n_result - n_x))
  }
  
  if (dbg) {
    
    metadata <- kwb.utils::noFactorDataFrame(
      left_table = c(name_x, n_x),
      key_columns = c(list_with_comma(by), ""),
      right_table = c(name_y, n_y),
      result_table = c(name, n_result)
    )
    
    rownames(metadata) <- c("name", "n_rows")
    
    write_markdown_chapter(
      kable_translated(metadata, row.names = TRUE), 
      level = dbg, 
      caption_key = "left_joining"
    )
  }
  
  fetch_with_postfix <- function(name, postfix) {
    kwb.utils::selectColumns(result, paste0(name, postfix), drop = FALSE)
  }
  
  comparable <- function(df) kwb.utils::defaultIfNA(as.character(df[[1L]]), "")
  
  for (name in common) {
    
    values_x <- fetch_with_postfix(name, ".x")
    values_y <- fetch_with_postfix(name, ".y")
    
    differs <- comparable(values_x) != comparable(values_y)
    
    if (message_anyway || any(differs)) {
      message_text(
        "n_differences", sum(differs), names(values_x), names(values_y)
      )
    }
  }
  
  structure(result, metadata = if (dbg) metadata)
}

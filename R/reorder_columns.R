# reorder_columns --------------------------------------------------------------
reorder_columns <- function(
  df, selection = "first", filename = "selections.csv", group_pairs = TRUE,
  dbg = TRUE
)
{
  #kwb.prep:::assign_objects()

  # All available column names
  all_columns <- names(df)
  
  # Group columns so that columns that belong together are next to each other
  if (group_pairs) {
    all_columns <- kwb.utils::pairwise(all_columns)
  }

  # Names of columns to appear first
  first <- get_selection(file = config_file(filename), column = selection, 
                         dbg = dbg)

  if (length(missing <- setdiff(first, all_columns))) {

    message_if(dbg, get_text(
      "unknown_columns_in_selection", 
      selection, filename, eol_collapsed("- ", missing)
    ))

    first <- setdiff(first, missing)
  }

  #kwb.utils::moveColumnsToFront(df, first)
  kwb.utils::selectColumns(
    df, 
    kwb.utils::moveToFront(all_columns, first), 
    drop = FALSE
  )
}

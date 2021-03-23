# reorder_columns --------------------------------------------------------------
reorder_columns <- function(
  df, selection = "first", filename = "selections.csv", group_pairs = TRUE
)
{
  #kwb.prep:::assign_objects()

  # Group columns so that columns that belong together are next to each other
  all_columns <- if (group_pairs) {
    kwb.utils::pairwise(names(df))
  } else {
    names(df)
  }

  # Names of columns to appear first
  first <- get_selection(file = config_file(filename), column = selection)

  if (length(missing <- setdiff(first, all_columns))) {

    message(get_text(
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

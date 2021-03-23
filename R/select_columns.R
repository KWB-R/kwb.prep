# select_columns ---------------------------------------------------------------
select_columns <- function(
  df, columns, indices = NULL, dbg = 0L, name = NULL, drop = FALSE
)
{
  if (dbg) {
    
    name <- getname(name, substitute(df))
    
    write_markdown_chapter(
      x = to_markdown_enum(columns),
      caption = if (name == ".") {
        get_text("select_columns")
      } else {
        get_text("select_columns_from", name)
      },
      level = dbg
    )
  }

  result <- kwb.utils::selectColumns(df, columns, drop = drop)

  if (is.null(indices)) {
    return(result)
  }

  check_indices(indices, nrow(df))

  if (length(dim(result)) == 2L) {

    result[indices, ]

  } else {

    result[indices]
  }
}

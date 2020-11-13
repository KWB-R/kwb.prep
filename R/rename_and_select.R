# rename_and_select ------------------------------------------------------------
#' @keywords internal
rename_and_select <- function(
  x, renamings, columns = as.character(renamings), dbg = 1L,
  name = deparse(substitute(x))
)
{
  if (dbg) {
    
    metadata <- kwb.utils::noFactorDataFrame(
      selected_column = columns,
      original_column = names(renamings)
    )
    
    metadata %>%
      kable_translated() %>%
      write_markdown_chapter(
        caption = if (name == ".") {
          get_text("select_rename_columns")
        } else {
          get_text("select_rename_columns_from", name)
        },
        level = dbg
      )
  }
  
  x %>%
    rename_columns(renamings, dbg = FALSE) %>%
    select_columns(columns, dbg = FALSE)
}


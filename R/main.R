# copy_column ------------------------------------------------------------------
copy_column <- function(df, to, from, indices = NULL, ...)
{
  set_column(df, to, from = from, indices = indices, ...)
}

# create_missing_column --------------------------------------------------------
# TODO: Merge with kwb.utils::hsAddMissingCols() and use that one
create_missing_column <- function(df, column, value = NA)
{
  stopifnot(is.data.frame(df), is.character(column), length(column) == 1L)
  
  if (column %in% colnames(df)) {
    return(df)
  }
  
  kwb.utils::catAndRun(
    get_text("creating_missing_column", column),
    df[[column]] <- value
  )
  
  df
}

# delete_one_value_columns -----------------------------------------------------
delete_one_value_columns <- function(df)
{
  kwb.utils::catAndRun(
    get_text("deleting_constant_columns"),
    Filter(function(x) n_unique(x) > 1L, df)
  )
}

# remove_columns ---------------------------------------------------------------
remove_columns <- function(
  x, columns = NULL, reason = NULL, ..., dbg. = TRUE, key = NULL
)
{
  configs <- read_args("remove_columns", dbg = FALSE)
  
  if (! is.null(key)) {
    
    config <- kwb.utils::selectElements(configs, key)
    
    return(remove_columns(
      x,
      columns = config$columns,
      reason = config$reason,
      pattern = config$pattern
    ))
  }
  
  before <- names(x)
  
  x <- kwb.utils::removeColumns(x, columns, ..., dbg = FALSE)
  
  removed <- setdiff(before, after <- names(x))
  
  if (! dbg. || ! length(removed) && ! in_development_mode()) {
    return(x)
  }
  
  content <- if (n_removed <- length(removed)) {
    
    md_enum <- to_markdown_enum(removed, collapse = TRUE)
    
    if (is.null(reason)) {
      get_text("columns_removed", n_removed, md_enum)
    } else {
      get_text("columns_removed_reason", n_removed, get_text(reason), md_enum)
    }
    
  } else {
    
    get_text("no_columns_removed")
  }
  
  write_markdown_chapter(content, "removing_columns", level = dbg.)
  
  x
}

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

# rename_columns ---------------------------------------------------------------
rename_columns <- function(
  x, renamings = NULL, dbg = 3L, name = deparse(substitute(x))
)
{
  before <- names(x)
  
  x <- kwb.utils::renameColumns(x, renamings)
  
  if (dbg && any(differs <- before != (after <- names(x)))) {
    
    write_markdown_chapter(
      knitr::kable(cbind(von = before[differs], nach = after[differs])),
      caption = get_text("renaming_columns", newline_collapsed(name)),
      level = dbg
    )
  }
  
  x
}

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

# safe_row_bind ----------------------------------------------------------------
#' @keywords internal
safe_row_bind <- function(
  x, y,
  name_x = deparse(substitute(x)),
  name_y = deparse(substitute(y)),
  dbg = 3L
)
{
  if (dbg) {
    
    metadata <- kwb.utils::noFactorDataFrame(
      table_name = c(name_x, name_y),
      n_rows = c(nrow(x), nrow(y)),
      n_cols = c(ncol(x), ncol(y))
    )
    
    write_markdown_chapter(
      kable_translated(metadata),
      caption_key = "row_bind",
      level = dbg
    )
  }
  
  kwb.utils::safeRowBind(x, y)
}

# set_columns ------------------------------------------------------------------
set_columns <- function(x, ..., dbg = 1L, name = deparse(substitute(x)))
{
  if (dbg) {
    
    write_markdown_chapter(
      to_markdown_enum(names(list(...))),
      caption = if (name == ".") {
        get_text("calculating_new_columns")
      } else {
        get_text("calculating_new_columns_in", name)
      },
      level = dbg
    )
  }
  
  kwb.utils::setColumns(x, ..., dbg = FALSE)
}

# stop_on_duplicates -----------------------------------------------------------
stop_on_duplicates <- function(data, columns, dbg = 3L)
{
  if (dbg) {
    
    write_markdown_chapter(
      get_text("no_duplicates", list_with_comma(columns)),
      caption_key = "duplicate_check",
      level = dbg
    )
  }
  
  invisible(data)
}

# split_by_columns -------------------------------------------------------------
split_by_columns <- function(df, columns, ...)
{
  split(df, kwb.utils::selectColumns(df, columns, drop = FALSE), ...)
}

# unique_rows ------------------------------------------------------------------
#' @keywords internal
unique_rows <- function(x, dbg = 2L)
{
  #kwb.prep::assign_objects()
  #x = iris; dbg = 2L
  stopifnot(is.data.frame(x))
  
  before <- nrow(x)
  x <- unique(x)
  after <- nrow(x)
  
  if (dbg) {
    
    removed <- before - after
    
    metadata <- kwb.utils::noFactorDataFrame(
      rows_before = before,
      rows_after = after,
      rows_removed = removed,
      rows_removed_percent = round(kwb.utils::percentage(removed, before), 1L),
      key_columns = list_with_comma(names(x))
    )
    
    metadata %>%
      kable_translated() %>%
      write_markdown_chapter("unique_rows", level = dbg)
  }
  
  x
}

# write_data_frame_info --------------------------------------------------------
write_data_frame_info <- function(
  x, level = 3L, name = deparse(substitute(x)),
  wide = get_option("table_structure_wide"),
  top_n = get_option("table_structure_top_n")
)
{
  write_markdown_chapter(
    c(
      get_text("table_dimesion", nrow(x), ncol(x)),
      kable_data_frame_structure(x, wide = wide, top_n = top_n)
    ),
    caption = get_text("structure_of", name),
    level = level
  )
  
  invisible(x)
}

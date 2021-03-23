# compare_data_frames ----------------------------------------------------------
compare_data_frames <- function(a, b, level = 3L)
{
  #kwb.prep::assign_objects()
  set_class <- function(x) kwb.utils::addClass(x, "data_frame_diff")
  
  diffs <- compare_data_frames_1(a, b)
  
  if (level == 1L)
    return(set_class(diffs))

  diffs <- c(
    kwb.utils::removeElements(diffs, "modified"),
    split_modified(kwb.utils::selectElements(diffs, "modified"))
  )
  
  if (level == 2L)
    return(set_class(diffs))
  
  compare <- function(x) merge(
    x, compare_columns(a, b, x[["column"]]), by = "column"
  )
  
  diffs[["modified_value"]] <- compare(diffs[["modified_value"]])
  diffs[["modified_type"]] <- compare(diffs[["modified_type"]])

  if (level == 3L)
    return(set_class(diffs))

  stop_("Currently only level = 1 or level = 2 or level = 3 is implemented.")
}

# print.data_frame_diff --------------------------------------------------------
#' Print Result of Data Frame Comparison
#' 
#' @param x object of class "data_frame_diff"
#' @param \dots currently not used
#' @export
print.data_frame_diff <- function(x, ...)
{
  stopifnot(inherits(x, "data_frame_diff"))
  
  diffs <- kwb.utils::createAccessor(x)
  
  list_first <- function(x) list_with_comma(x[[1L]])
  
  cat("Added:", list_first(diffs("added")), "\n")
  cat("Removed:", list_first(diffs("removed")), "\n")
  
  if (is.null(x$modified_value)) {
    return()
  }
  
  kwb.utils::printIf(TRUE, diffs("modified_value"), "Modified values")
  kwb.utils::printIf(TRUE, diffs("modified_type"), "Modified types")
  
  if (is.null(diffs("modified_value")[["n_diff_gt_1p"]])) {
    return()
  }

  cat(
    "\nn_diff_gt_1p: Number of values with absolute difference greater than\n",
    "  one percent abs((a - b) / b) > 0.01"
  )
}

# compare_data_frames_1 --------------------------------------------------------
compare_data_frames_1 <- function(a, b, level = 1L)
{
  #kwb.prep::assign_objects()
  
  # Find identical columns
  idents <- find_identical_columns(a, b)
  
  # Helper function to remove identical columns
  reduce <- function(x, y) kwb.utils::removeColumns(x, y[nzchar(y)])
  
  # Remove identical columns from the input data frames
  a_red <- reduce(a, idents$column_x)
  b_red <- reduce(b, idents$column_y)
  
  # There must not be duplicated columns any more
  stopifnot(nrow(find_identical_columns(a_red, b_red)) == 0L)
  
  # Use safe_merge() to arrange columns with corresponding names
  info <- safe_merge(
    x = a_red[logical(), ],
    y = b_red[logical(), ],
    dbg = FALSE,
    metadata = TRUE
  ) %>%
    kwb.utils::getAttribute("metadata") %>%
    kwb.utils::selectColumns(c("column_x", "column_y", "type_x", "type_y")) %>%
    (
      function(x) x %>% kwb.utils::setColumns(
        in_both = nzchar(x$column_x) & nzchar(x$column_y),
        same_type = x$type_x == x$type_y,
        dbg = FALSE
      )
    )
  
  #View(info)
  is_removed <- nzchar(info$column_x) & ! info$in_both
  is_added <- nzchar(info$column_y) & ! info$in_both
  is_common <- ! is_removed & ! is_added
  
  list(
    identical = idents,
    removed = info[is_removed, c("column_x", "type_x")],
    added = info[is_added, c("column_y", "type_y")],
    modified = info[is_common, ] %>%
      kwb.utils::removeColumns(c("column_y", "in_both")) %>%
      kwb.utils::renameColumns(list(column_x = "column"))
  )
}

# split_modified ---------------------------------------------------------------
split_modified <- function(modified)
{
  column <- "same_type"
  same_type <- kwb.utils::selectColumns(modified, column)
  modified <- kwb.utils::removeColumns(modified, column)
  
  list(
    
    modified_value = modified[same_type, ] %>%
      kwb.utils::removeColumns("type_y") %>% 
      kwb.utils::renameColumns(list(type_x = "type")),
    
    modified_type = modified[! same_type, ]
  )
}

# compare_columns --------------------------------------------------------------
compare_columns <- function(a, b, columns)
{
  stats::setNames(nm = columns) %>%
    lapply(function(column) compare_vectors(a[[column]], b[[column]])) %>%
    kwb.utils::rbindAll(nameColumn = "column", namesAsFactor = FALSE) %>%
    kwb.utils::removeColumns(c("ident", "same_mode")) %>%
    kwb.utils::moveColumnsToFront("column")
}

# compare_vectors --------------------------------------------------------------
compare_vectors <- function(x, y)
{
  n_na <- function(xx) sum(is.na(xx))
  both_numeric <- is.numeric(x) && is.numeric(y)
  
  kwb.utils::noFactorDataFrame(
    ident = identical(x, y),
    same_mode = identical(mode(x), mode(y)),
    diff_n_na = n_na(y) - n_na(x),
    max_abs_diff = if (both_numeric) {
      as.numeric(max(abs(x - y), na.rm = TRUE))
    } else {
      NA_real_
    },
    n_diff_gt_1p = if (both_numeric) {
      sum(abs((x - y) / x) > 0.01, na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
}

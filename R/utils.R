# assign_objects ---------------------------------------------------------------

#' Assign all Objects of This Package in the Global Environment
#' @export
assign_objects <- function() kwb.utils::assignPackageObjects("kwb.prep")

# cat_if -----------------------------------------------------------------------
cat_if <- kwb.utils::catIf

# cat_text ---------------------------------------------------------------------
cat_text <- function(x, ...)
{
  cat(get_text(x, ...))
}

# catf -------------------------------------------------------------------------
catf <- function(...)
{
  cat(sprintf(...))
}

# check_indices ----------------------------------------------------------------
check_indices <- function(indices, max_index, max_length = max_index)
{
  stopifnot(is.integer(indices))
  stopifnot(! any(is.na(indices)))
  stopifnot(all(indices > 0))
  stopifnot(all(indices <= max_index))
  stopifnot(length(indices) <= max_length)
  stopifnot(! anyDuplicated(indices))
}

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

# current_year ------------------------------------------------------------------
# copy of kwb.datetime::currentYear
current_year <- function()
{
  as.integer(format(Sys.Date(), "%Y"))
}

# delete_one_value_columns -----------------------------------------------------
delete_one_value_columns <- function(df)
{
  kwb.utils::catAndRun(
    get_text("deleting_constant_columns"),
    Filter(function(x) n_unique(x) > 1L, df)
  )
}

# get_dbg ----------------------------------------------------------------------
get_dbg <- function(default = 1L)
{
  getOption("sema_prep_app_dbg", default)
}

# get_year_number --------------------------------------------------------------
get_year_number <- function(x)
{
  stopifnot(inherits(x, "Date") || inherits(x, "POSIXct"))

  as.integer(format(x, "%Y"))
}

# grep_value -------------------------------------------------------------------
grep_value <- function(pattern, x)
{
  grep(pattern, x, value = TRUE)
}

# hide_non_changing ------------------------------------------------------------
hide_non_changing <- function(df, column, keep = NULL)
{
  indices_show <- indices_of_change(kwb.utils::selectColumns(df, column))
  indices_hide <- setdiff(seq_len(nrow(df)), indices_show)

  df[indices_hide, setdiff(names(df), keep)] <- NA

  df
}

# indices_of_change ------------------------------------------------------------
indices_of_change <- function(x)
{
  c(1L, which(x[-length(x)] != x[-1L]) + 1L)
}

# is_csv_database --------------------------------------------------------------
is_csv_database <- function(path)
{
  dir.exists(path)
}

# is_empty_output --------------------------------------------------------------
is_empty_output <- function(output)
{
  length(output) == 0L || all(output == "")
}

# list_to_lookup_table ---------------------------------------------------------
list_to_lookup_table <- function(x, nm = NULL)
{
  result <- kwb.utils::toLookupTable(
    List = x,
    as.twoColumnTable = TRUE,
    stringsAsFactors = FALSE
  )

  if (is.null(nm)) {
    return(result)
  }

  stats::setNames(result, nm)
}

# list_with_comma --------------------------------------------------------------
list_with_comma <- function(x)
{
  paste(x, collapse = ", ")
}

# log_console ------------------------------------------------------------------
log_console <- function(file, expr, width = 1000L, ..., append = TRUE)
{
  op <- options(width = width)
  on.exit(options(op))

  con <- file(file, "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  utils::capture.output(result <- eval(expr), file = con)

  result
}

# main_class -------------------------------------------------------------------
main_class <- function(x)
{
  class(x)[1L]
}

# message_if -------------------------------------------------------------------
message_if <- function(check, ...)
{
  if (check) {
    message(...)
  }
}

# message_if -------------------------------------------------------------------
message_if <- function(condition, ...)
{
  if (condition) {
    message(...)
  }
}

# message_text -----------------------------------------------------------------
message_text <- function(x, ...)
{
  message(get_text(x, ...))
}

# n_na: Number of NA values ----------------------------------------------------
n_na <- function(x)
{
  # TODO: kwb.sema:::numNaInColumn() still required?
  sum(is.na(x))
}

# n_unique: Number of unique values --------------------------------------------
n_unique <- function(x)
{
  length(unique(x))
}

# n_unique_in_column: Number of unique values in a data frame column -----------
n_unique_in_column <- function(df, column)
{
  n_unique(select_columns(df, column))
}

# named_seq_along --------------------------------------------------------------
named_seq_along <- function(x)
{
  stats::setNames(seq_along(x), x)
}

# newline_collapsed ------------------------------------------------------------
newline_collapsed <- function(x)
{
  paste(x, collapse = "\n")
}

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

# print_kable ------------------------------------------------------------------
print_kable <- function(...)
{
  print(knitr::kable(...))
}

# print_to_string --------------------------------------------------------------
print_to_string <- function(x)
{
  newline_collapsed(utils::capture.output(print(x)))
}

# remove_columns ---------------------------------------------------------------
remove_columns <- function(
  x, columns = NULL, reason = NULL, ..., dbg. = TRUE, key = NULL
)
{
  configs <- list(
    duplicated_in_cctv = list(
      columns = c("n_rehab", "n_repairs", "ConstructionDate_raw")
    ),
    intermediate = list(
      pattern = "((Ground|Invert)Level(Up|Down)stream)|LENGTH_FROM_COORDS",
      reason = "reason_intermediate"
    ),
    intermediate_lookup = list(
      pattern = "Insp$",
      reason = "reason_intermediate_lookup"
    ),
    this_year = list(
      columns = "THIS_YEAR",
      reason = "reason_this_year"
    )
  )

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

# run_cached -------------------------------------------------------------------
run_cached <- function(name, expr, dbg = FALSE)
{
  object <- kwb.utils:::get_cached(name)

  if (is.null(object)) {

    write_enum_if(dbg, "not_in_file_cache", name)

    object <- kwb.utils:::cache_and_return(
      try(eval(expr, envir = -1)), 
      name = name
    )

    write_enum_if(dbg, "object_cached_in_file", name)

  } else {

    write_enum_if(dbg, "loading_from_file_cache", name)
  }

  object
}

# save_as ----------------------------------------------------------------------
save_as <- function(x, name, file = NULL)
{
  file <- kwb.utils::defaultIfNULL(file, file.path(
    tempdir(), sprintf("object_%s.RData", name)
  ))

  save(
    list = name,
    envir = list2env(stats::setNames(list(x), name)),
    file = file
  )

  structure(invisible(x), file = file)
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


# set_dbg ----------------------------------------------------------------------
set_dbg <- function(dbg = 1L)
{
  options(sema_prep_app_dbg = dbg)
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

# find_string_constants --------------------------------------------------------

#' Show String Constants Used in R Scripts
#'
#' @export
find_string_constants <- function()
{
  #remotes::install_github("kwb-r/kwb.code@dev")
  kwb.file::add_file_info(
    kwb.code::get_string_constants_in_scripts(
      root = "./R", FUN = kwb.code:::fetch_string_constants_2
    )
  )
}

# split_by_columns -------------------------------------------------------------
split_by_columns <- function(df, columns, ...)
{
  split(df, kwb.utils::selectColumns(df, columns, drop = FALSE), ...)
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...) stop(..., call. = FALSE)

# stopf ------------------------------------------------------------------------
stopf <- function(fmt, ...)
{
  stop_(sprintf(fmt, ...))
}

# table_any_na -----------------------------------------------------------------
table_any_na <- function(x, y, a, b)
{
  table(x, y, useNA = "ifany", dnn = c(a, b))
}

# to_rcode_snippet -------------------------------------------------------------
to_rcode_snippet <- function(x)
{
  c(
    "```{r eval = FALSE}",
    x,
    "```"
  )
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

# write_csv_file ---------------------------------------------------------------
write_csv_file <- function(x, file, dbg = TRUE)
{
  kwb.utils::catAndRun(
    get_text("writing_csv", file),
    dbg = dbg,
    writeStandardCsv(x, file, na = "")
  )
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

# write_enum -------------------------------------------------------------------
write_enum <- function(x, ...)
{
  writeLines(to_markdown_enum(get_text(x, ...)))
}

# write_enum_if ----------------------------------------------------------------
write_enum_if <- function(check, x, ...)
{
  if (check) {
    write_enum(x, ...)
  }
}

# write_lines_utf8 -------------------------------------------------------------
write_lines_utf8 <- function(text, file, ...)
{
  stopifnot(is.character(file), length(file) == 1L)

  con <- file(file, "w", encoding = "UTF-8")
  on.exit(close(con))

  writeLines(text, con, ...)
}


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

# cross_if ---------------------------------------------------------------------
cross_if <- function(check)
{
  ifelse(check, "x", "")
}

# current_year -----------------------------------------------------------------
# copy of kwb.datetime::currentYear
current_year <- function()
{
  as.integer(format(Sys.Date(), "%Y"))
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

# getname ----------------------------------------------------------------------
getname <- function(name, x) kwb.utils::defaultIfNULL(name, deparse(x))

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

# names_which ------------------------------------------------------------------
names_which <- function(x)
{
  names(which(x))
}

# newline_collapsed ------------------------------------------------------------
newline_collapsed <- function(x)
{
  paste(x, collapse = "\n")
}

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

# print_to_string --------------------------------------------------------------
print_to_string <- function(x)
{
  newline_collapsed(utils::capture.output(print(x)))
}

# run_cached -------------------------------------------------------------------
run_cached <- function(name, expr = NULL, dbg = FALSE)
{
  object <- kwb.utils:::get_cached(name, dbg = FALSE)

  if (is.null(object)) {

    write_enum_if(dbg, "not_in_file_cache", name)

    if (! is.null(expr)) {
      
      object <- kwb.utils:::cache_and_return(
        try(eval(expr, envir = -1)), 
        name = name
      )
      
      write_enum_if(dbg, "object_cached_in_file", name)
    }

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

# save_as_if -------------------------------------------------------------------
save_as_if <- function(x, do_save, name, file = NULL)
{
  if (do_save) {
    save_as(x, name = name, file = file)
  }
  
  x
}

# set_dbg ----------------------------------------------------------------------
set_dbg <- function(dbg = 1L)
{
  options(sema_prep_app_dbg = dbg)
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

# write_csv_file ---------------------------------------------------------------
write_csv_file <- function(x, file, dbg = TRUE)
{
  kwb.utils::catAndRun(
    get_text("writing_csv", file),
    dbg = dbg,
    writeStandardCsv(x, file, na = "")
  )
}

# write_lines_utf8 -------------------------------------------------------------
write_lines_utf8 <- function(text, file, ...)
{
  stopifnot(is.character(file), length(file) == 1L)

  con <- file(file, "w", encoding = "UTF-8")
  on.exit(close(con))

  writeLines(text, con, ...)
}


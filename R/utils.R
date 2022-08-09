# app_file ----------------------------------------------------------------------
app_file <- function(...)
{
  extdata_file(...)
}

# assign_objects ---------------------------------------------------------------
#' Assign Package Functions to the Global Environment
#' 
#' This function provides all (also non-exported) function definitions of this 
#' package in the Global environment. This is useful for debugging the code
#' of a function that calls non-exported functions.
#' 
#' @export
assign_objects <- kwb.utils::createFunctionAssignObjects("kwb.prep")

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

# count_unique -----------------------------------------------------------------
count_unique <- function(x)
{
  stats::aggregate(.n ~ ., data = cbind(x, .n = 1L), FUN = length)
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

# flatten_data_frame_lists -----------------------------------------------------
flatten_data_frame_lists <- function(x, prefix = NULL)
{
  result <- lapply(names(x), function(name) prefix_names(x[[name]], name))
  
  prefix_names(do.call(c, result), prefix)
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

# get_separator ----------------------------------------------------------------
get_separator <- function(name, config, element = "sep")
{
  kwb.utils::defaultIfNULL(
    kwb.utils::getListNode(config, name)[[element]],
    default = get_default_element(config, element)
  )
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

# n_unique_in_column: Number of unique values in a data frame column -----------
n_unique_in_column <- function(df, column)
{
  kwb.utils::nUnique(kwb.utils::selectColumns(df, column))
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

# eol_collapsed ----------------------------------------------------------------
eol_collapsed <- function(...)
{
  paste0(..., collapse = "\n")
}

# package_file -----------------------------------------------------------------
package_file <- function(...)
{
  system.file(..., package = "kwb.prep")
}

# prefix_names -----------------------------------------------------------------
prefix_names <- function(x, prefix = NULL)
{
  if (is.null(prefix)) {
    return(x)
  }
  
  stats::setNames(x, paste0(prefix, "_", names(x)))
}

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

# print_to_string --------------------------------------------------------------
print_to_string <- function(x)
{
  eol_collapsed(utils::capture.output(print(x)))
}

# remove_prefix ----------------------------------------------------------------
remove_prefix <- function(x, prefix)
{
  gsub(paste0("^", prefix, "_"), "", x)
}

# round_numeric ----------------------------------------------------------------
round_numeric <- function(x, digits = 2L)
{
  is_numeric <- sapply(x, inherits, "numeric")
  x[is_numeric] <- lapply(x[is_numeric], round, digits)
  x
}

# run_cached -------------------------------------------------------------------
run_cached <- function(name, expr = NULL, dbg = FALSE)
{
  # Globally disable caching
  #Sys.setenv(KWB_PREP_DISABLE_CACHE = "TRUE")
  
  # If environment variable KWB_PREP_DISABLE_CACHE is "TRUE", return the 
  # evaluated expression, without any caching
  if (Sys.getenv("KWB_PREP_DISABLE_CACHE") == "TRUE") {
    return(try(eval(expr, envir = -1)))
  }
  
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
save_as <- function(x, name, file = NULL, dbg = TRUE)
{
  file <- kwb.utils::defaultIfNULL(file, file.path(
    tempdir(), sprintf("object_%s.RData", name)
  ))

  kwb.utils::catAndRun(
    dbg = dbg,
    sprintf(
      "Saving '%s' as '%s' to\n  '%s'", 
      deparse(substitute(x)), name, file
    ), 
    expr = save(
      list = name,
      envir = list2env(stats::setNames(list(x), name)),
      file = file
    )
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

# temp_import_dir --------------------------------------------------------------
temp_import_dir <- function()
{
  kwb.utils::tempSubdirectory("kwb.prep")
}

# read_yaml_file ---------------------------------------------------------------
read_yaml_file <- function(file, dbg = TRUE)
{
  kwb.utils::catAndRun(
    sprintf("Reading yaml file '%s'", file),
    yaml::read_yaml(kwb.utils::safePath(file)),
    dbg = dbg
  )
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...) stop(..., call. = FALSE)

# stopf ------------------------------------------------------------------------
stopf <- kwb.utils::stopFormatted

# stop_no_more_msaccess_support ------------------------------------------------
stop_no_more_msaccess_support <- function()
{
  clean_stop("MS Access databases are not supported any more.")
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



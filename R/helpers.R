# assign_objects ---------------------------------------------------------------

#' Provide all Objects of kwb.prep in the Global Environment
#' 
#' @export
assign_objects <- function()
{
  kwb.utils::assignPackageObjects("kwb.prep")
}

# field_renamings_to_lookup_table ----------------------------------------------
field_renamings_to_lookup_table <- function(x)
{
  list_to_lookup_table(x, nm = c("original_field", "internal_field"))
}

# check_grouping_if ------------------------------------------------------------
check_grouping_if <- function(x, check, column_raw, column_cat)
{
  if (check) {
    checkGrouping(x, column_raw, column_cat)
  }

  x
}

# check_number_of_unique_if ----------------------------------------------------
check_number_of_unique_if <- function(x, check, columns)
{
  if (check) {
    checkNumberOfUnique(x, columns)
  }

  x
}

# copy_column ------------------------------------------------------------------
copy_column <- function(df, to, from, indices = NULL, ...)
{
  set_column(df, to, from = from, indices = indices, ...)
}

# get_column_separator ---------------------------------------------------------
get_column_separator <- function(id = NULL)
{
  sep <- get_option("column_separator")
  
  if (is.character(sep)) {
    return(sep)
  }
  
  if (is.null(id) || is.null(sep[[id]])) {
    return(kwb.utils::selectElements(sep, "default"))
  }
  
  sep[[id]]
}

# get_lower_extension ----------------------------------------------------------

#' Lower Case Extension of a File
#' 
#' @param file file path or file name
#' @export
#' @examples 
#' get_lower_extension("abc.XYZ")
get_lower_extension <- function(file)
{
  tolower(kwb.utils::fileExtension(file))
}

# get_option -------------------------------------------------------------------
get_option <- function(
  name = NULL, file = config_file("options.yml"), dbg = FALSE
)
{
  if (FALSE) {

    values <- list(
      warn_about_missing_table_fields = TRUE
    )

    yaml::write_yaml(values, file = "inst/extdata/config/options.yml")
  }

  values <- yaml::read_yaml(kwb.utils::safePath(file))

  if (is.null(name))
    return(values)

  value <- kwb.utils::selectElements(values, name)

  if (dbg)
    message_text("using_option_value", name, value)

  value
}

# get_user_config_dir ----------------------------------------------------------
get_user_config_dir <- function(default = NULL)
{
  kwb.utils::defaultIfNULL(get_global("user_config_dir"), default)
}

# in_development_mode ----------------------------------------------------------
in_development_mode <- function()
{
  get_option("mode") == "development"
}

# msaccess_to_r_type -----------------------------------------------------------
msaccess_to_r_type <- function()
{
  list(
    DATETIME = "POSIXct",
    DECIMAL = "numeric",
    DOUBLE = "numeric",
    INTEGER = "integer",
    REAL = "numeric",
    VARCHAR = "character"
  )
}

# read_args --------------------------------------------------------------------
read_args <- function(name, dbg = TRUE)
{
  file <- config_file(sprintf("args_%s.yml", name))
  
  kwb.utils::catAndRun(
    get_text("reading_args", name, file),
    dbg = dbg,
    yaml::read_yaml(file)
  )
}

# read_config ------------------------------------------------------------------
read_config <- function(file_name, dbg = FALSE, file = NULL)
{
  file <- kwb.utils::defaultIfNULL(file, config_file(file_name))
  
  extension <- get_lower_extension(file)
  
  if (extension == "yml") {
    
    read_yaml_file(file, dbg = dbg)
    
  } else if (extension == "csv") {
    
    read_csv_file(file, dbg = dbg, encoding = "Latin-1")
    
  } else stop_(
    
    "Configuration file does not have extension .yml or .csv:\n",
    file
  )
}

# read_filter_criteria ---------------------------------------------------------
#' @importFrom yaml read_yaml
read_filter_criteria <- function(
  file = config_file("filter_criteria.yml"), dbg = TRUE
)
{
  kwb.utils::catAndRun(
    paste(get_text("reading_filter_criteria"), file), dbg = dbg, {
    lapply(yaml::read_yaml(file), FUN = unlist)
  })
}

# read_internal_types ----------------------------------------------------------
read_internal_types <- function(file = NULL, dbg = FALSE)
{
  if (is.null(file)) {
    file <- config_file("internal-types.csv", in_package = FALSE)
  }
  
  result <- read_csv_file(file, dbg = dbg)

  fetch <- kwb.utils::createAccessor(result)

  stopifnot(! anyDuplicated(fetch("internal_field")))

  types <- fetch("internal_type")

  stopifnot(all(! is.na(types)))

  result
}

# replace_by_condition ---------------------------------------------------------
#' @keywords internal
replace_by_condition <- function(
  df, group, path = NULL, dbg = 1L, 
  filename = "replace-by-condition.csv"
)
{
  #path=NULL
  #kwb.prep::assign_objects()
  result <- suppressMessages(replaceByCondition(
    df = df, group = group, dbg = FALSE, file = kwb.utils::defaultIfNULL(
      path, config_file(filename, in_package = FALSE)
    )
  ))

  metadata <- result %>%
    kwb.utils::getAttribute("metadata") %>%
    kwb.utils::moveColumnsToFront(c("n_replaced", "target", "replacement"))

  metadata %>%
    kable_translated(caption = get_text("replacements_by_condition", group)) %>%
    write_markdown_chapter("replace_specials", level = dbg)

  structure(result, metadata = metadata)
}

# set_origin -------------------------------------------------------------------
set_origin <- function(data, origin = kwb.utils::getAttribute(data, "origin"))
{
  cbind(data, origin = origin, stringsAsFactors = FALSE)
}

# set_user_config_dir ----------------------------------------------------------
set_user_config_dir <- function(path)
{
  set_global("user_config_dir", kwb.utils::safePath(path))
}

# split_by_columns -------------------------------------------------------------
split_by_columns <- function(df, columns, ...)
{
  split(df, kwb.utils::selectColumns(df, columns, drop = FALSE), ...)
}

# summary_with_forced_na -------------------------------------------------------
summary_with_forced_na <- function(x)
{
  result <- summary(x, digits = 3)

  if (any(is.na(x))) {
    return(result)
  }

  c(result, "NA's" = 0)
}


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
read_internal_types <- function(dbg = FALSE)
{
  result <- read_csv_file(config_file("internal-types.csv"), dbg = dbg)

  fetch <- kwb.utils::createAccessor(result)

  stopifnot(! anyDuplicated(fetch("internal_field")))

  types <- fetch("internal_type")

  stopifnot(all(! is.na(types)))

  result
}

# replace_by_condition ---------------------------------------------------------
#' @keywords internal
replace_by_condition <- function(df, group, path = NULL, dbg = 1L)
{
  #path=NULL
  #kwb.prep::assign_objects()

  result <- suppressMessages(replaceByCondition(
    df = df, group = group, dbg = FALSE, file = kwb.utils::defaultIfNULL(
      path, config_file("replace_invalid.csv", must_exist = TRUE)
    )
  ))

  metadata <- result %>%
    kwb.utils::getAttribute("metadata") %>%
    kwb.utils::moveColumnsToFront(c(
      "n_replaced", "target_column", "replacement"
    ))

  metadata %>%
    kable_translated(
      caption = get_text("replacements_invalid_csv", group)
    ) %>%
    write_markdown_chapter("replace_specials", level = dbg)

  structure(result, metadata = metadata)
}

# set_origin -------------------------------------------------------------------
set_origin <- function(data, origin = kwb.utils::getAttribute(data, "origin"))
{
  cbind(data, origin = origin, stringsAsFactors = FALSE)
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


# apply_filter -----------------------------------------------------------------
apply_filter <- function(
  x, element, length_column = NULL, dbg = 2L, name = deparse(substitute(x)),
  config = read_filter_criteria(dbg = FALSE)
)
{
  output <- utils::capture.output(
    result <- applyFilter(x, config, element, length_column)
  )

  write_markdown_chapter(
    to_rcode_snippet(output),
    level = dbg,
    caption = if (name == ".") {
      get_text("applying_filter", element)
    } else {
      get_text("applying_filter_to", element, name)
    }
  )

  if (! is.null(length_column)) {

    writeLines(printFilterTable(
      kwb.utils::getAttribute(result, "details.filter")
    ))
  }

  result
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

# left_join --------------------------------------------------------------------
left_join <- function(
  x, y, by, use_dplyr = TRUE, message_anyway = TRUE, check = TRUE, 
  dbg = get_dbg(), 
  name_x = deparse(substitute(x)), 
  name_y = deparse(substitute(y)),
  name = paste0(name_x, "_", name_y)
)
{
  non_by <- function(df) setdiff(names(df), by)
    
  if (length(common <- intersect(non_by(x), non_by(y)))) {
    message_text(
      "common_columns", name_x, name_y,
      kwb.utils::stringList(common, collapse = "\n- ")
    )
  }

  result <- if (use_dplyr) {

    dplyr::left_join(x, y, by = by)

  } else {

    merge(x, y, by = by, all.x = TRUE)
  }

  # Check (if requested): The number of rows must not change!
  n_x <- nrow(x)
  n_y <- nrow(y)
  n_result <- nrow(result)
  
  if (check && n_result != n_x) {
    stop_(get_text("merging_failed", name_x, name_y, n_result - n_x))
  }

  if (dbg) {
    
    metadata <- kwb.utils::noFactorDataFrame(
      left_table = c(name_x, n_x),
      join_by = c(list_with_comma(by), ""),
      right_table = c(name_y, n_y),
      result_table = c(name, n_result)
    )
    
    rownames(metadata) <- c("name", "n_rows")

    write_markdown_chapter(
      kable_translated(metadata, row.names = TRUE), 
      level = dbg, 
      caption_key = "left_joining"
    )
  }
  
  fetch_with_postfix <- function(name, postfix) {
    kwb.utils::selectColumns(result, paste0(name, postfix), drop = FALSE)
  }

  comparable <- function(df) kwb.utils::defaultIfNA(as.character(df[[1L]]), "")

  for (name in common) {

    values_x <- fetch_with_postfix(name, ".x")
    values_y <- fetch_with_postfix(name, ".y")

    differs <- comparable(values_x) != comparable(values_y)

    if (message_anyway || any(differs)) {
      message_text(
        "n_differences", sum(differs), names(values_x), names(values_y)
      )
    }
  }

  structure(result, metadata = metadata)
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

# summary_with_forced_na -------------------------------------------------------
summary_with_forced_na <- function(x)
{
  result <- summary(x, digits = 3)

  if (any(is.na(x))) {
    return(result)
  }

  c(result, "NA's" = 0)
}


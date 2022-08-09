# check_or_set_column_types_if -------------------------------------------------
check_or_set_column_types_if <- function(
  df, check, field_config = NULL, dbg = TRUE
)
{
  if (! check) {
    return(df)
  }

  check_or_set_column_types(df, field_config, dbg)
}

# check_or_set_column_types ----------------------------------------------------
# @param field_config list with one entry per column. Each entry is a list that
# may contain a "format" element (string describing the Date or time format)
check_or_set_column_types <- function(df, field_config = NULL, dbg = TRUE)
{
  #kwb.prep::assign_objects()
  #df=result;dbg=TRUE

  mismatches <- get_type_mismatches(df, dbg = in_development_mode())

  conversions <- split(mismatches, select_columns(mismatches, "internal_type"))

  #conversion <- conversions[[2L]]

  for (conversion in conversions) {

    #conversion <- conversions[[1L]]
    fetch <- kwb.utils::createAccessor(conversion)
    columns <- fetch("internal_field")

    df[columns] <- lapply(
      columns,
      convert_column,
      data = df,
      type = fetch("internal_type")[1L],
      field_config = field_config,
      dbg = dbg
    )
  }

  df
}

# convert_column ---------------------------------------------------------------
# @param field_config list with one entry per column. Each entry is a list that
# may contain a "format" element (string describing the Date or time format)
convert_column <- function(data, column, type, field_config = NULL, dbg = TRUE)
{
  x <- kwb.utils::selectColumns(data, column)

  if (is.null(field_config)) {

    conversion <- NULL
    format <- NULL

  } else {

    type_config <- kwb.utils::getListNode(field_config, column)
    conversion <- type_config[["conversion"]]
    format <- type_config[["format"]]
  }

  if (! is.null(format)) {
    clean_stop(
      "'format' is not supported any more. Please use a 'converter' expression"
    )
  }

  kwb.utils::catAndRun(
    get_text("converting", column, kwb.utils::mainClass(x), type),
    dbg = dbg,
    expr = if (! is.null(conversion)) {

      eval(parse(text = conversion, keep.source = FALSE), list(x = x))

    } else if (type == "numeric") {

      kwb.utils::hsChrToNum(x, country = "de")

    } else {

      args <- list(x)

      # Add "format" argument if the type is date or time
      if (type %in% c("Date", "POSIXct") && ! is.null(format)) {
        args <- c(args, list(format = format))
      }

      do.call(paste0("as.", type), args)
    }
  )
}

# get_type_mismatches ----------------------------------------------------------

#' @keywords internal
get_type_mismatches <- function(df, dbg = TRUE)
{
  NAME_TYPE <- unname(unlist(get_text(c("column_name", "current_type"))))

  # Helper function
  exclude_missing <- function(type_info, is_missing) {

    if (dbg) {

      write_markdown_chapter(
        kwb.prep::kable_no_rows(stats::setNames(
          type_info[is_missing, 1:2],
          NAME_TYPE
        )),
        caption_key = "no_type_info_found"
      )
    }

    type_info[! is_missing, ]
  }

  # Helper function
  report_unchanged <- function(type_info, type_differs) {

    write_markdown_chapter(
      kwb.prep::kable_no_rows(stats::setNames(
        type_info[! type_differs, 1:2],
        NAME_TYPE
      )),
      caption_key = "no_type_conversion_required"
    )
  }

  main_classes <- lapply(df, kwb.utils::mainClass)

  current_type_lookup <- list_to_lookup_table(main_classes, c(
    "internal_field", "current_type"
  ))

  type_file <- kwb.utils::safePath(get_user_config_dir(), "internal-types.csv")
  internal_types <- read_internal_types(file = type_file)

  type_info <- left_join(
    x = current_type_lookup,
    y = internal_types,
    by = "internal_field",
    dbg = FALSE
  )

  fetch <- kwb.utils::createAccessor(type_info)

  is_missing <- is.na(fetch("internal_type"))

  if (any(is_missing)) {

    # Remove rows with missing information on the internal type from type_info
    type_info <- exclude_missing(type_info, is_missing)

    # Update the accessor function
    fetch <- kwb.utils::createAccessor(type_info)
  }

  type_differs <- fetch("current_type") != fetch("internal_type")

  if (dbg > 1L && any(! type_differs)) {
    report_unchanged(type_info, type_differs)
  }

  kwb.utils::resetRowNames(type_info[type_differs, ])
}

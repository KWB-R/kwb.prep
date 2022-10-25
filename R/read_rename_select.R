# read_rename_select -----------------------------------------------------------
read_rename_select <- function(
  path_db,
  table_name,
  config = read_config("csv_config.yml"),
  strict = TRUE,
  set_types = TRUE,
  dbg = 1L
)
{
  #kwb.prep::assign_objects()
  #table_name = "rehabilitation";strict=TRUE;dbg=TRUE;set_types = TRUE

  check_table_name(table_name)

  # Provide table field renamings (empty list may be returned!)
  renamings <- get_renamings_from_config(config, table_name)

  # Get names of available table fields
  available <- get_table_fields(path_db, table_name, config)

  # Select only existing columns if strict is FALSE
  if (!strict) {
    renamings <- remove_missing_renamings(renamings, available = available)
  }

  if (!is_csv_database(path_db)) {
    stop_no_more_msaccess_support()
  }

  format <- get_file_format(config, table_name)

  result <- if (format == "csv") {
    
    read_rename_select_csv(
      path_db,
      table_name = table_name,
      config = config,
      renamings = renamings,
      set_types = set_types,
      dbg = dbg
    )
    
  } else if (format == "dbf") {
    
    read_rename_select_dbf(
      path_db,
      table_name = table_name,
      config = config,
      renamings = renamings,
      set_types = set_types,
      dbg = dbg
    )

  } else {
    
    stop_file_format_not_supported(format, where = "read_rename_select()")
  }
  
  # Add columns that are not required but for which default values are given
  add_non_required_columns(result, config, table_name)
}

# remove_missing_renamings -----------------------------------------------------
remove_missing_renamings <- function(renamings, available)
{
  wanted <- names(renamings)
  missing <- setdiff(wanted, available)

  if (length(missing)) {
    all_renamings <- renamings
    renamings <- renamings[intersect(wanted, available)]
  }

  if (! get_option("warn_about_missing_table_fields")) {
    return(renamings)
  }

  cat_text("missing_fields")

  print_kable(
    row.names = FALSE,
    field_renamings_to_lookup_table(all_renamings[missing])
  )

  renamings
}

# read_rename_select_csv -------------------------------------------------------
read_rename_select_csv <- function(
  path_db, table_name, config, renamings, set_types = TRUE,
  encoding = "Latin-1", set_empty_string_to_na = TRUE, dbg = TRUE
)
{
  #encoding = "Latin-1";set_empty_string_to_na = TRUE;dbg = TRUE

  # Get names of available table fields
  available <- get_table_fields(path_db, table_name, config)
  colClasses <- get_column_classes2(table_name, config, available)
  sep <- get_separator(table_name, config, element = "sep")
  dec <- get_separator(table_name, config, element = "dec")

  file <- get_csv_file_path(path_db, table_name, config)

  result <- read_csv_file(
    file,
    sep = sep,
    dec = dec,
    #dec = ifelse(sep == ",", ".", ","),
    colClasses = colClasses,
    encoding = encoding,
    set_empty_string_to_na = set_empty_string_to_na,
    dbg = dbg
  )

  result <- replace_na_strings_with_na(
    result, 
    na.strings = kwb.utils::defaultIfNULL(config$default$na.strings, "")
  )
  
  result <- rename_and_select(
    result, 
    renamings, 
    dbg = dbg
  )

  if (set_types) {
    
    result <- check_or_set_column_types(
      result, 
      field_config = kwb.utils::getListNode(config, paste0(table_name, "/fields")), 
      dbg = dbg
    )
  }

  result
}

# read_rename_select_dbf -------------------------------------------------------
read_rename_select_dbf <- function(...)
{
  stop("Not yet implemented: read_rename_select_dbf()")
}

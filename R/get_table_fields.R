# get_table_fields -------------------------------------------------------------
get_table_fields <- function(path_db, table_name, config)
{
  if (! is_csv_database(path_db)) {
    stop_no_more_msaccess_support()
  }

  check_table_name(table_name)

  format <- get_file_format(config, table_name)

  if (format == "csv") {
    
    get_table_fields_csv(path_db, table_name, config)
    
  } else if (format == "dbf") {

    get_table_fields_dbf(path_db, table_name, config)    
    
  } else {

    stop_file_format_not_supported(format, where = "get_table_fields()")
  }
}

# get_file_format --------------------------------------------------------------
get_file_format <- function(config, table_name)
{
  table_config <- kwb.utils::selectElements(config, table_name)
  
  format <- table_config[["format"]]
  
  if (is.null(format)) {
    file <- kwb.utils::selectElements(table_config, "file")
    format <- lower_file_extension(file)
  }

  format  
}

# get_csv_file_path ------------------------------------------------------------
get_csv_file_path <- function(path_db, table_name, config)
{
  file_name <- kwb.utils::getListNode(config, paste0(table_name, "/file"))
  file.path(path_db, file_name)
}

# get_table_fields_csv ---------------------------------------------------------
get_table_fields_csv <- function(path_db, table_name, config)
{
  #kwb.prep::assign_objects()

  get_csv_header_fields(
    file = get_csv_file_path(path_db, table_name, config),
    sep = get_separator(table_name, config, "sep")
  )
}

# get_table_fields_dbf ---------------------------------------------------------
get_table_fields_dbf <- function(path_db, table_name, config)
{
  file <- get_csv_file_path(path_db, table_name, config)
  file <- kwb.utils::safePath(file)
  names(foreign::read.dbf(file, as.is = TRUE))
}

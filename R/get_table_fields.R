# get_table_fields -------------------------------------------------------------
get_table_fields <- function(path_db, table_name, ...)
{
  if (! is_csv_database(path_db)) {
    stop_no_more_msaccess_support()
  }

  check_table_name(table_name)

  get_table_fields_csv(path_db, table_name, ...)
}

# get_table_fields_csv ---------------------------------------------------------
get_table_fields_csv <- function(path_db, table_name, config)
{
  #sema.prep.app:::assign_objects()

  get_csv_header_fields(
    file = get_csv_file_path(path_db, table_name, config),
    sep = get_separator(table_name, config, "sep")
  )
}

# get_csv_file_path ------------------------------------------------------------
get_csv_file_path <- function(path_db, table_name, config)
{
  file_name <- kwb.utils::getListNode(config, paste0(table_name, "/file"))
  file.path(path_db, file_name)
}

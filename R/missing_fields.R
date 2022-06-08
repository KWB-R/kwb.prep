# missing_fields ---------------------------------------------------------------
missing_fields <- function(
  path_db,
  config,
  table_names = expected_tables(config)
)
{
  #sema.prep.app::assign_objects()

  if (kwb.utils::hasZeroLength(table_names)) {
    return(list())
  }

  fields <- lapply(
    X = stats::setNames(nm = table_names),
    FUN = missing_fields_one,
    path_db = path_db,
    config = config
  )

  fields[lengths(fields) > 0L]
}

# missing_fields_one -----------------------------------------------------------
missing_fields_one <- function(path_db, config, table_name)
{
  #sema.prep.app:::assign_objects()
  #path_db <- "~/tmp/sema-berlin_tool-prep/runs/2021-02-02_210108_semadaten_v1.0"
  #config <- kwb.prep:::read_config("csv_config.yml")
  #table_name <- "pipes"
  #kwb.prep:::set_global("user_config_dir", config_file())
  check_table_name(table_name)

  # Names of available fields in the table/csv file of given name
  available <- get_table_fields(path_db, table_name, config)

  # Info on required fields with original and internal name
  required <- get_renamings_from_config(config, table_name, all = FALSE)

  # Which of the required fields are missing?
  is_missing <- ! names(required) %in% available

  if (! any(is_missing)) {
    return(character())
  }

  unlist(kwb.utils::revertListAssignments(required[is_missing]))
}

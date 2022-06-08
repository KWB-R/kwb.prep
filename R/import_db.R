# import_db --------------------------------------------------------------------

#' Import CSV Files from ZIP File
#'
#' The function stops with an error message if the \code{file} does not have the
#' file extension ".zip" or if the zip file does not contain the expected csv
#' files or if a csv file does not contain all expected fields (columns).
#' Expected file names and field names are provided \code{config}). If
#' everything looks ok, the csv files in the zip file are extracted into a (new)
#' folder in the app's "run" directory. The app directory is provided in the
#' environment variable SEMA_BERLIN_PREP_APP_DIR.
#'
#' @param zip_file path to zip file containing csv files
#' @param config configuration object (list) describing the csv files
#' @param base_name base name of the folder to be created. The current date will
#'   also be encoded in the folder name. By default the base name of the zip
#'   file (file name without file extension) is used.
#' @export
import_db <- function(zip_file, config, base_name = basename(zip_file))
{
  #sema.prep.app::assign_objects();target_dir=NULL

  check_zip_extension(zip_file)

  check_missing_tables(zip_file, config)

  temp_dir <- temp_import_dir()

  #kwb.utils::hsOpenWindowsExplorer(temp_dir)

  unzip_archive(zip_file, temp_dir)

  # Check for missing fields
  check_missing_fields(temp_dir, config)

  import_dir <- get_path(
    "input_dir",
    app_dir = app_file(),
    date = Sys.Date(),
    import_name = file_to_import_name(base_name)
  )

  stopifnot(import_dir != "")

  run_dir <- kwb.utils::directoryName(import_dir)

  if (dir.exists(run_dir)) {
    cat(get_text("import_dir_exists", run_dir), "\n")
    return(import_dir)
    #stop_text("import_dir_exists", kwb.utils::fullWinPath(run_dir))
  }

  kwb.utils::createDirectory(import_dir, dbg = FALSE)

  kwb.utils::catAndRun(
    get_text(
      "copying_csv_from_to",
      kwb.utils::fullWinPath(temp_dir),
      kwb.utils::fullWinPath(import_dir)
    ),
    newLine = 3L,
    file.copy(dir(temp_dir, full.names = TRUE, recursive = TRUE), import_dir)
  )

  import_dir
}

# check_missing_tables ---------------------------------------------------------
check_missing_tables <- function(zip_file, config)
{
  tables <- missing_tables(zip_file, config)

  if (length(tables)) {

    # Strings giving the table names and the csv file names in parentheses
    strings <- sprintf("%s (%s)", tables, get_csv_filenames(config[tables]))

    stop_text("missing_tables", eol_collapsed("- ", strings))
  }
}

# check_missing_fields ---------------------------------------------------------
check_missing_fields <- function(target_dir, config)
{
  fields <- missing_fields(target_dir, config)

  if (length(fields)) {

    stop_text("missing_fields_in_csv", field_info_to_text(
      field_info = field_list_to_data_frame(fields),
      config = config,
      format = 1L # 2L
    ))
  }
}

# field_info_to_text -----------------------------------------------------------
field_info_to_text <- function(field_info, config, format = 1L)
{
  if (format == 1L) {
    table_names <- field_info[[1L]]
    field_names <- field_info[[3L]]
    indices <- match(table_names, names(config))
    file_names <- unname(get_csv_filenames(config, keep_empty = TRUE)[indices])
    return(paste(file_names, field_names, sep = ": ", collapse = "\n"))
  }

  if (format == 2L) {
    return(print_to_string(field_info))
  }

  stop("format must be either 1L or 2L")
}

# field_list_to_data_frame -----------------------------------------------------
field_list_to_data_frame <- function(fields)
{
  nameColumn <- "table_name"

  info <- kwb.utils::moveColumnsToFront(
    columns = nameColumn,
    kwb.utils::rbindAll(nameColumn = nameColumn, lapply(fields, function(x) {
      if (length(x))
        field_renamings_to_lookup_table(as.list(x))
    }))
  )

  translate_columns(info)
}

# file_to_import_name ----------------------------------------------------------
file_to_import_name <- function(x)
{
  kwb.utils::removeExtension(basename(x))
}

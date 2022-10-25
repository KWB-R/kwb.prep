# missing_tables ---------------------------------------------------------------
missing_tables <- function(zip_file, config)
{
  #kwb.prep::assign_objects()
  setdiff(expected_tables(config), contained_tables(zip_file, config))
}

# expected_tables --------------------------------------------------------------
expected_tables <- function(config)
{
  config <- remove_default(config)

  if (kwb.utils::hasZeroLength(config)) {
    return(character(0))
  }

  names(which(collect(config, "required", default = TRUE)))
}

# get_default_element ----------------------------------------------------------
get_default_element <- function(config, element)
{
  kwb.utils::getListNode(config, paste0("default/", element))
}

# remove_default ---------------------------------------------------------------
remove_default <- function(config)
{
  config[setdiff(names(config), "default")]
}

# contained_tables -------------------------------------------------------------

#' Names of "Tables" Provided in Zip Archive
#' 
#' @param zip_file path to zip archive file (.zip or .7z)
#' @param config csv configuration mapping table names to file names
#' @return vector of character containing the names of the tables for which 
#'   corresponding files have been found in the given zip archive
#' @export
#' @examples
#' kwb.prep::contained_tables(
#'   zip_file = kwb.prep:::invalid_zip("five-rows.zip"), 
#'   config = list(
#'     pipes = list(file = "Haltungen_SAN_utf8.csv"),
#'     trees = list(file = "Haltungen_Baeume_utf8.csv"),
#'     animals = list(file = "no-animals-there.csv")
#'   )
#' )
contained_tables <- function(zip_file, config)
{
  expected <- sapply(remove_default(config), kwb.utils::selectElements, "file")

  zipped_paths <- get_zipped_paths(zip_file)

  if (length(zipped_paths) == 0L) {
    stop_text("no_files_in_zip_file", zip_file)
  }

  names(expected)[expected %in% basename(zipped_paths)]
}

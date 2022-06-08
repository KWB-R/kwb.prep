# missing_tables ---------------------------------------------------------------
missing_tables <- function(zip_file, config)
{
  #sema.prep.app:::assign_objects()
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
contained_tables <- function(zip_file, config)
{
  expected <- sapply(remove_default(config), kwb.utils::selectElements, "file")

  zipped_paths <- get_zipped_paths(zip_file)

  if (length(zipped_paths) == 0L) {
    stop_text("no_files_in_zip_file", zip_file)
  }

  names(expected)[expected %in% basename(zipped_paths)]
}

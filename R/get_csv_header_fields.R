# get_csv_header_fields --------------------------------------------------------
get_csv_header_fields <- function(file, sep = guess_sep(file))
{
  file <- kwb.utils::safePath(file)

  fields <- strsplit(readLines(file, 1L), sep)[[1L]]
  
  gsub("[\"']", "", fields)
}

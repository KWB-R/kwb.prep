# get_csv_header_fields --------------------------------------------------------
get_csv_header_fields <- function(
  file, sep = kwb.prep:::guess_sep(file), method = 1L
)
{
  file <- kwb.utils::safePath(file)

  read <- function(header) {
    utils::read.table(file, sep = sep, nrows = 1L, header = header)
  }

  if (method == 1L) {
    return(unname(as.matrix(read(header = FALSE))[1L, ]))
  }

  names(read(header = TRUE))
}

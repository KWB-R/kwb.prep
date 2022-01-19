# read_csv_file ----------------------------------------------------------------
#' Read Data Frame From CSV File
#'
#' @param file path to csv file
#' @param sep Column separator character. Default: semicolon ";"
#' @param dec Decimal separator character. Default: comma ","
#' @param encoding file encoding string. Default: "UTF-8". Possible other value:
#'   "unknown"
#' @param na.strings strings occurring in the files representing NA (not
#'   available). Default: ""
#' @param \dots further arguments passed to
#'   \code{\link[data.table]{fread}}
#' @param set_empty_string_to_na if \code{TRUE} (the default is \code{FALSE}) 
#'   empty strings in character columns are replaced with \code{NA}
#' @param dbg if \code{TRUE} debug messages are shown
read_csv_file <- function(
  file, sep = get_column_separator(), dec = ",", encoding = "UTF-8",
  na.strings = "", ..., set_empty_string_to_na = FALSE, dbg = 1L
)
{
  #kwb.prep::assign_objects()
  #file <- "~/tmp/sema-berlin_db-export/LinerBWB_utf8.csv"
  metadata <- get_file_metadata(file)

  write_markdown_chapter(kable_translated(metadata), "read_csv_file", dbg)

  # See fakin.path.app:::read_csv_fread
  result <- as.data.frame(data.table::fread(
    file = kwb.utils::safePath(file),
    sep = sep,
    dec = dec,
    encoding = encoding,
    na.strings = na.strings
    , ...
  ))

  # Replace empty strings in character columns with NA
  if (set_empty_string_to_na) {
    is_char <- sapply(result, is.character)
    result[is_char] <- lapply(result[is_char], function(x) {
      x[x == ""] <- NA
      x
    })
  }
  
  structure(result, metadata = metadata)
}

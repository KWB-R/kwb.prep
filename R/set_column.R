# set_column -------------------------------------------------------------------

#' Set Column
#'
#' @param df data frame
#' @param column column
#' @param value value
#' @param indices row indices
#' @param from name of source column, optional
#' @param must_exist is column assumed to exist?
#' @export
#' @importFrom kwb.utils checkForMissingColumns
set_column <- function(
  df, column, value = NULL, indices = NULL, from = NULL, must_exist = TRUE
)
{
  stopifnot(is.data.frame(df))
  stopifnot(is.character(column), length(column) == 1L)

  if (must_exist) {
    kwb.utils::checkForMissingColumns(df, column)
  }

  if (is.null(value)) {

    stopifnot(is.character(from), length(from) == 1L)

    value <- select_columns(df, from, indices, drop = TRUE)
  }

  if (is.null(indices)) {

    df[[column]] <- value

    return(df)
  }

  check_indices(indices, nrow(df))

  if (is.null(df[[column]])) {
    df[[column]] <- NA
  }

  df[[column]][indices] <- value

  df
}

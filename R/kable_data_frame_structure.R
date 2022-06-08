# kable_data_frame_structure ---------------------------------------------------

#' @keywords internal
kable_data_frame_structure <- function(df, ...)
{
  info <- get_data_frame_structure(df, ...)

  old_options <- options(knitr.kable.NA = "")
  on.exit(options(old_options))

  kable_translated(info)
}

# get_data_frame_structure -----------------------------------------------------
get_data_frame_structure <- function(df, wide = TRUE, top_n = 3L)
{
  #kwb.prep::assign_objects()
  stopifnot(is.data.frame(df))

  unnamed_sapply <- function(x, fun, ...) unname(sapply(x, fun, ...))

  backbone <- kwb.utils::noFactorDataFrame(
    column = names(df),
    data_type = unnamed_sapply(df, kwb.utils::mainClass),
    n_na = unnamed_sapply(df, kwb.utils::nNA),
    n_distinct = unnamed_sapply(df, kwb.utils::nUnique)
  )

  if (wide) {
    return(cbind(
      backbone,
      most_frequent = unnamed_sapply(df, main_n, top_n),
      stringsAsFactors = FALSE
    ))
  }

  table_columns <- c("value", "frequency")

  top_x <- lapply(df, function(x) {
    freq <- main_n(x, top_n, collapsed = FALSE)
    if (length(freq)) {
      stats::setNames(as.data.frame(freq), table_columns)
    } else {
      do.call(data.frame, as.list(stats::setNames(rep(NA, 2L), table_columns)))
    }
  })

  left_join(
    x = backbone,
    y = kwb.utils::rbindAll(top_x, "column"),
    by = "column",
    check = FALSE
  ) %>%
    hide_non_changing("column", keep = table_columns)
}

# main_n -----------------------------------------------------------------------
main_n <- function(x, n, collapsed = TRUE)
{
  freq <- sort(table(x), decreasing = TRUE)
  freq_n <- freq[seq_len(min(n, length(freq)))]

  if (! collapsed) {
    return(freq_n)
  }

  list_with_comma(sprintf("%s (%dx)", names(freq_n), unname(freq_n)))
}

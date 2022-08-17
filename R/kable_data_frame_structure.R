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
      most_frequent = unnamed_sapply(df, main_n, top_n, useNA = "ifany"),
      stringsAsFactors = FALSE
    ))
  }

  table_columns <- c("value", "frequency")

  top_x <- lapply(df, function(x) {
    freq <- main_n(x, top_n, collapsed = FALSE, useNA = "ifany")
    stats::setNames(nm = table_columns, object = if (length(freq)) {
      kwb.utils::namedVectorToDataFrame(freq)
    } else {
      do.call(data.frame, as.list(c(NA, NA)))
    })
  })

  left_join(
    x = backbone,
    y = kwb.utils::rbindAll(top_x, "column"),
    by = "column",
    check = FALSE,
    dbg = FALSE
  ) %>%
    hide_non_changing("column", keep = table_columns)
}

# main_n -----------------------------------------------------------------------
main_n <- function(x, n, collapsed = TRUE, useNA = "no")
{
  freq <- sort(table(x, useNA = useNA), decreasing = TRUE)
  freq_n <- freq[seq_len(min(n, length(freq)))]

  if (! collapsed) {
    return(freq_n)
  }

  list_with_comma(sprintf("%s (%dx)", names(freq_n), unname(freq_n)))
}

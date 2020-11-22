# normalise_prediction ---------------------------------------------------------
normalise_prediction <- function(
  x, column_id, column_value, column_model = NULL, model_to_status = NULL,
  default_status = "predicted"
)
{
  # Select (and rename) the relevant columns: <column_id>, value, model
  prediction <- x %>%
    kwb.utils::selectColumns(c(column_id, column_value)) %>%
    cbind(stringsAsFactors = FALSE, if (is.null(column_model)) {
      "unknown"
    } else {
      kwb.utils::selectColumns(x, column_model)
    }) %>%
    stats::setNames(c(column_id, "value", "model"))

  # Convert model name to status name and provide it in new column "status"
  prediction_status <- if (is.null(model_to_status)) {

    kwb.utils::setColumns(prediction, status = default_status)

  } else {

    dplyr::left_join(x = prediction, by = "model", y = list_to_lookup_table(
      model_to_status, nm = c("model", "status")
    ))
  }
  
  # Check that there is a status name for each model name
  stopifnot(! anyNA(prediction_status$status))
  
  prediction_status
}

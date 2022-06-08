# check_csv_config -------------------------------------------------------------
check_csv_config <- function(config)
{
  if (is.null(config)) {
    stop_text("empty_csv_config")
  }
}

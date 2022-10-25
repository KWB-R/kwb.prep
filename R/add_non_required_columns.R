# add_non_required_columns -----------------------------------------------------
add_non_required_columns <- function(data, config, table_name)
{
  stopifnot(is.data.frame(data))

  field_config <- kwb.utils::getListNode(config, paste0(table_name, "/fields"))

  # Names of columns to be added
  columns <- setdiff(
    names(which(!is_required_field(field_config))),
    names(data)
  )

  for (column in columns) {
    path <- paste0(column, "/default")
    data[[column]] <- kwb.utils::getListNode(field_config, path)
  }

  data
}

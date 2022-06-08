# delete_one_value_columns -----------------------------------------------------
delete_one_value_columns <- function(df)
{
  kwb.utils::catAndRun(
    get_text("deleting_constant_columns"),
    Filter(function(x) kwb.utils::nUnique(x) > 1L, df)
  )
}

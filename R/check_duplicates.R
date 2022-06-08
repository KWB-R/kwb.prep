# check_duplicates -------------------------------------------------------------
check_duplicates <- function(x)
{
  is_duplicated <- duplicated(x)
  
  if (any(is_duplicated)) clean_stop(
    "There are duplicates: ", paste(unique(x[is_duplicated]), collapse = ", ")
  )
}

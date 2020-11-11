# assign_all -------------------------------------------------------------------

#' Assign all Objects of This Package in the Global Environment
#' @export
assign_all <- function() kwb.utils::assignPackageObjects("kwb.prep")

# cat_if -----------------------------------------------------------------------
cat_if <- kwb.utils::catIf

# check_indices ----------------------------------------------------------------
check_indices <- function(indices, max_index, max_length = max_index)
{
  stopifnot(is.integer(indices))
  stopifnot(! any(is.na(indices)))
  stopifnot(all(indices > 0))
  stopifnot(all(indices <= max_index))
  stopifnot(length(indices) <= max_length)
  stopifnot(! anyDuplicated(indices))
}

# message_if -------------------------------------------------------------------
message_if <- function(condition, ...)
{
  if (condition) {
    message(...)
  }
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...) stop(..., call. = FALSE)

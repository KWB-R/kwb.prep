# assign_all -------------------------------------------------------------------

#' Assign all Objects of This Package in the Global Environment
#' @export
assign_all <- function() kwb.utils::assignPackageObjects("kwb.prep")

# stop_ ------------------------------------------------------------------------
stop_ <- function(...) stop(..., call. = FALSE)

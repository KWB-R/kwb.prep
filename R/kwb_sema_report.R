# moved from kwb.sema (report.R)

# fieldSummary -----------------------------------------------------------------
fieldSummary <- function(x, groupBy, lengthColumn = "", na = "Unknown")
{
  for (column in groupBy) {
    values <- kwb.utils::selectColumns(x, column)
    levels(values) <- c(levels(values), na)
    x[[column]] <- kwb.utils::defaultIfNA(values, na)
  }
  
  y <- stats::aggregate(kwb.utils::toFormula("Count", groupBy), cbind(x, Count = 1), length)
  
  if (lengthColumn != "") {
    
    y1 <- stats::aggregate(kwb.utils::toFormula(lengthColumn, groupBy), x, sum, na.rm = TRUE)
    
    y <- merge(y, y1, by = groupBy)
  }
  
  y$Percentage <- 100 * kwb.utils::selectColumns(y, "Count") / nrow(x)
  
  y
}

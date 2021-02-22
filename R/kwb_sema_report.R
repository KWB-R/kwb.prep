# moved from kwb.sema (report.R)

# fieldSummary -----------------------------------------------------------------
fieldSummary <- function(x, groupBy, lengthColumn = "", na = "Unknown")
{
  #column=groupBy[1L];lengthColumn=lengthColumn2;na="Unknown"
  if (nrow(x) == 0L) {
    
    message(
      "The data frame given to fieldSummary() has no rows!"
    )
    
    return(do.call(data.frame, args = c(as.list(x[groupBy]), list(
      Count = integer(), Percentage = numeric()
    ))))
  }
  
  for (column in groupBy) {
    
    values <- kwb.utils::selectColumns(x, column)
    levels(values) <- c(levels(values), na)
    x[[column]] <- kwb.utils::defaultIfNA(values, na)
  }
  
  y <- stats::aggregate(
    kwb.utils::toFormula("Count", groupBy), 
    data = cbind(x, Count = 1), 
    FUN = length
  )
  
  if (lengthColumn != "") {
    
    y1 <- stats::aggregate(
      kwb.utils::toFormula(lengthColumn, groupBy), 
      data = x, 
      FUN = sum, 
      na.rm = TRUE
    )
    
    y <- merge(y, y1, by = groupBy)
  }
  
  y$Percentage <- 100 * kwb.utils::selectColumns(y, "Count") / nrow(x)
  
  y
}

# printFilterTable -------------------------------------------------------------
printFilterTable <- function(x)
{
  round1 <- function(x) round(x, 1L)
  
  x$Length.go <- round1(kwb.utils::selectColumns(x, "Length.go") / 1000)
  x$Percentage.go <- round1(kwb.utils::selectColumns(x, "Percentage.go"))
  
  x$Length.keep <- round1(kwb.utils::selectColumns(x, "Length.keep") / 1000)
  x$Percentage.keep <- round1(kwb.utils::selectColumns(x, "Percentage.keep"))
  
  columns <- c("CleaningStep",
               "Count.go", "Percentage.go", "Length.go",
               "Count.keep", "Percentage.keep", "Length.keep")
  
  x <- kwb.utils::renameColumns(x[, columns], list(
    CleaningStep = "Cleaning Step",
    Count.go = "# match",
    Length.go = "km match",
    Percentage.go = "% match",
    Count.keep = "# keep",
    Length.keep = "km keep",
    Percentage.keep = "% keep"
  ))
  
  knitr::kable(x)
}

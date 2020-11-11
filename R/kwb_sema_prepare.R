# cumulateMatchMatrix ----------------------------------------------------------
cumulateMatchMatrix <- function(matchMatrix)
{
  out <- matchMatrix
  
  for (i in seq_len(ncol(out) - 1) + 1) {
    out[, i] <- out[, i-1] & matchMatrix[, i]
  }
  
  out  
}

# keepOrGoSummary --------------------------------------------------------------
keepOrGoSummary <- function(i, lengthColumn, x.base, y.base, criteriaNames)
{
  conditionID <- paste0("C", i)
  
  if (is.null(lengthColumn)) {
    lengthColumn2 <- ""
  } else {
    lengthColumn2 <- "Length"
    
    # aggregate() within fieldSummary() does not work as expected if all Lengths
    # are NA. So we temporarily set NA to 0.0
    x.base$Length <- kwb.utils::defaultIfNA(kwb.utils::selectColumns(x.base, lengthColumn2), 0.0)
    y.base$Length <- kwb.utils::defaultIfNA(kwb.utils::selectColumns(y.base, lengthColumn2), 0.0)
  }
  
  x.out <- fieldSummary(x = x.base, groupBy = conditionID, lengthColumn2)
  y.out <- fieldSummary(x = y.base, groupBy = conditionID, lengthColumn2)
  
  # Select the information on the excluded and of the remaining
  x.out <- x.out[x.out[, conditionID] == FALSE, ]
  y.out <- y.out[y.out[, conditionID] == TRUE,  ]
  
  # If the condition was always met generate a zero-entry
  if (nrow(x.out) == 0) {
    x.out <- rbind(x.out, kwb.utils::toLookupTable(names(x.out), rep(0, ncol(x.out))))
  }
  
  # If the condition was never met generate a zero-entry
  if (nrow(y.out) == 0) {
    y.out <- rbind(y.out, kwb.utils::toLookupTable(names(y.out), rep(0, ncol(y.out))))
  }
  
  names(x.out)[-1] <- paste0(names(x.out)[-1], ".go")
  names(y.out)[-1] <- paste0(names(y.out)[-1], ".keep")
  
  out <- cbind(CleaningStep = criteriaNames[i], x.out, y.out)
  
  kwb.utils::removeColumns(out, conditionID)
}

# mergeTwoVectors --------------------------------------------------------------
mergeTwoVectors <- function (x, y)
{
  # Start with the mean (hoping that both values are available)
  z <- (x + y) / 2
  
  # Which elements of z are NA (-> NA in x or NA in y)
  isNA <- is.na(z)
  
  # Replace NA in z with one of the two depths (pmax = parallel maximum)
  z[isNA] <- pmax(x[isNA], y[isNA], na.rm = TRUE)
  
  z
}

# doTypeConversions ------------------------------------------------------------
doTypeConversions <- function(Data, typeConversion = NULL, dbg = TRUE)
{
  stopifnot(is.list(typeConversion))
  
  if (! is.null(typeConversion) && length(typeConversion) > 0) {
    
    for (FUN in names(typeConversion)) {
      
      kwb.utils::catIf(dbg, sprintf("Running '%s' on:\n", FUN))
      
      for (column in typeConversion[[FUN]]) {
        kwb.utils::catIf(dbg, "--", column, "...")
        Data[[column]] <- do.call(FUN, list(kwb.utils::selectColumns(Data, column)))
        kwb.utils::catIf(dbg, "ok.\n")
      }
    }
  } else {
    message("No type conversions specified.")
  }
  
  Data
}

# countAndShowNAsIfSpecified ---------------------------------------------------
countAndShowNAsIfSpecified <- function(Data, columns = NULL)
{
  if (columns == "_ALL_") {
    columns <- names(Data)
  }
  
  if (! is.null(columns) && ! all(is.na(columns))) {
    for (column in columns) {
      cat(sprintf(
        "Number of NA in column '%s': %d\n",
        column, 
        sum(! stats::complete.cases(kwb.utils::selectColumns(Data, column)))
      ))
    }
  } else {
    message("No specification for showing NAs")
  }
}

# .getNames --------------------------------------------------------------------
.getNames <- function(x, defaultNames = paste0("x_", seq_along(x)))
{
  names.x <- kwb.utils::defaultIfNULL(names(x), defaultNames)
  isEmpty <- names.x == ""
  names.x[isEmpty] <- defaultNames[isEmpty]
  names.x
}

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

# getNames ---------------------------------------------------------------------
getNames <- function(x, defaultNames = paste0("x_", seq_along(x)))
{
  names.x <- kwb.utils::defaultIfNULL(names(x), defaultNames)
  isEmpty <- names.x == ""
  names.x[isEmpty] <- defaultNames[isEmpty]
  names.x
}

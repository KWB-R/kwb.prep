# doRegroupings ----------------------------------------------------------------

#' Apply Regrouping of Values in a Data Frame
#' 
#' @param Data data frame
#' @param regroup.actual default: settings$regroup.actual
#' @param regroup.config default: settings$regroup.config
#' @param settings list of settings that may contain the elements
#'   \code{regroup.actual} and \code{regroup.config}
#' @param checkRemaining if TRUE (default) it is checked if all values that
#'   occurred in a column to be regrouped have been considered in the regrouping
#' @param to.factor if \code{TRUE} the new values are converted to
#'   \code{factor}. The default is \code{FALSE}.
#' @param dbg if \code{TRUE} (default) debug messages are shown
#'   
#' @export
#' 
doRegroupings <- function(
  Data, regroup.actual = kwb.utils::selectElements(settings, "regroup.actual"),
  regroup.config = kwb.utils::selectElements(settings, "regroup.config"),
  settings = NULL, checkRemaining = TRUE, to.factor = FALSE, dbg = TRUE
)
{
  #checkRemaining=TRUE;dbg=TRUE
  
  if (kwb.utils::isNullOrEmpty(regroup.actual)) {
    
    message("No regroupings specified -> nothing to do.")
  }
  
  i <- 1
  
  #i<-13;actual <- regroup.actual[[i]]
  
  for (actual in regroup.actual) {
    
    message(sprintf("%02d. %s", i, actual$to))
    
    if (actual$from %in% names(Data)) {
      
      Data <- applyRegrouping(Data, actual, regroup.config, to.factor, dbg)
      
    } else {
      
      message(
        "Column ", kwb.utils::hsQuoteChr(actual$from), " does not exist. ", 
        "I skip this actual regrouping."
      )
    }
    
    i <- i + 1
    
  } # end of for (actual in regroup.actual)
  
  Data
}

# applyRegrouping --------------------------------------------------------------
applyRegrouping <- function(Data, actual, regroup.config, to.factor, dbg)
{
  kwb.utils::catIf(dbg, sprintf(
    paste(
      "Creating column '%s' from column '%s' using", 
      "config '%s' with %s ...\n"
    ),
    actual$to, actual$from, actual$name, actual$labels
  ))
  
  values <- kwb.utils::selectColumns(Data, actual$from)
  
  config <- regroup.config[[actual$name]]
  
  if (! is.null(config)) {
    
    values <- regroupedValues(
      values = values, config = config, labels = actual$labels, 
      to.factor = to.factor, dbg = dbg
    )
    
  } else {
    
    message(sprintf(
      "No config '%s' available -> Just copying...", actual$name
    ))
  }
  
  Data[[actual$to]] <- values
  
  kwb.utils::catIf(dbg, sprintf("-- Column '%s' ok.\n", actual$to))
  
  Data
}

# regroupedValues --------------------------------------------------------------

#' Regroup Values According to Configuration
#' 
#' @param values vector of values
#' @param config configuration (list) describing how to regroup. If the list
#'   contains an element \code{breaks} the function \code{\link{groupByBreaks}}
#'   is called to group values together that belong to the same intervals that
#'   are defined by the breaks. Otherwise the list must contain an element 
#'   \code{values} and an element of the name given in \code{labels} (default: 
#'   "labels1"). These are given to the function \code{\link{regroup}} that 
#'   performs a "value to label"-regrouping.
#' @param labels default: "labels1"
#' @param to.factor if \code{TRUE} the new values are converted to
#'   \code{factor}. The default is \code{FALSE}.
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' 
#' @export
#' 
regroupedValues <- function(
  values, config = NULL, labels = "labels1", to.factor = FALSE, dbg = TRUE
)
{
  # If config is NULL or an empty list, return the original values
  if (is.null(config) || length(config) == 0) {
    
    kwb.utils::catIf(dbg, sprintf(
      "Returning original values for %s.\n", deparse(substitute(values))
    ))
    
    return (values)
  }
  
  # If no breaks are given, use regroup
  if (is.null(config$breaks)) {
    
    assignments <- toAssignments2(
      values = kwb.utils::selectElements(config, "values"),
      labels = kwb.utils::selectElements(config, labels)
    )
    
    values.new <- regroup(
      x = values, 
      assignments = assignments$assignments, 
      ignore.case = config$ignore.case, # may be NULL
      to.factor = to.factor
    )
    
    # check for untreated values if not specified differently
    check <- kwb.utils::defaultIfNULL(config$checkRemaining, TRUE)
    
    if (check) {
      
      kwb.utils::catIf(dbg, "-- Checking for untreated values... ")
      
      ok <- messageOnRemaining(
        x = sort(unique(values)), assignments = assignments
      )
      
      kwb.utils::catIf(dbg && ok, "ok.\n")
    }    
    
  } else { # If breaks are given, use groupByBreaks
    
    values.new <- groupByBreaks(
      x = values, 
      breaks = kwb.utils::selectElements(config, "breaks"),
      values = kwb.utils::selectElements(config, labels),
      right = ifelse (is.null(config$right), TRUE, config$right),
      to.factor = to.factor
    )
  }
  
  values.new
}

# toAssignments2 ---------------------------------------------------------------
toAssignments2 <- function(values, labels)
{
  groups <- unique(labels)
  
  assignments <- lapply(groups, FUN = function(group) values[labels == group])
  
  list(assignments = stats::setNames(assignments, nm = groups))
}

# regroup ----------------------------------------------------------------------

#' Assign Values to Groups of Values
#' 
#' @param x vector of values
#' @param assignments list of assignments of the form \<key\> = \<values\> with
#'   \<values\> being a vector of elements to be looked up in \code{x} and to be
#'   replaced with \<key\> in the output vector
#' @param ignore.case if \code{TRUE} the case is ignored when comparing values
#' @param to.factor if \code{TRUE} the new values are converted to
#'   \code{factor}. The default is \code{FALSE}.
#'   
#' @return vector with as many elements as there are elements in \code{x}. The
#'   vector contains \<key\> at positions where the elements in \code{x} appeared
#'   in the vector \<values\> of a \<key\> = \<values\> assignment of
#'   \code{assignments}
#' 
#' @export
#' 
#' @examples 
#' regroup(c("A", "B", "C", "D"), assignments = list(
#'   "AB" = c("A", "B"),
#'   "CD" = c("C", "D")
#' ))
#'   
#' regroup(c("A", "B", "C", "D", "E", "A"), assignments = list(
#'   "AB" = c("A", "B"),
#'   "CD" = c("C", "D")
#' ))
#' 
regroup <- function(x, assignments, ignore.case = NULL, to.factor = FALSE)
{
  # Set default for ignore.case
  ignore.case <- kwb.utils::defaultIfNULL(ignore.case, FALSE)
  
  xnew <- rep(NA, times = length(x))
  
  keys <- names(assignments)
  
  for (key in keys) {
    
    matching <- if (ignore.case) {
      
      tolower(x) %in% tolower(assignments[[key]])
      
    } else {
      
      x %in% assignments[[key]]
    }
    
    if (any(matching)) {
      
      xnew[matching] <- key
    }
  }
  
  typeConverted(xnew, to.factor, factorLevels = keys)
}

# typeConverted ----------------------------------------------------------------
typeConverted <- function(x, to.factor, factorLevels)
{
  # Convert "<NA>" to NA
  x[x == "<NA>"] <- NA
  
  # If all new values look like numeric convert to numeric
  if (all(kwb.utils::hsValidValue(kwb.utils::hsTrim(x), lng = "en"))) {
    
    as.numeric(x)
    
  } else if (isTRUE(to.factor)) {
    
    factor(x, factorLevels)
    
  } else {
    
    x
  }
}

# messageOnRemaining -----------------------------------------------------------
messageOnRemaining <- function(x, assignments)
{
  remaining <- setdiff(x, unlist(assignments, use.names = FALSE))
  
  ok <- (length(remaining) == 0)
  
  if (! ok) {
    
    message("Untreated value(s):")
    
    print(remaining)
  }
  
  ok
}

# groupByBreaks ----------------------------------------------------------------

#' Group Values Belonging to Intervals
#' 
#' Group values together that belong to the same intervals being defined by 
#' breaks
#' 
#' @param x vector of values or a data frame. If \code{x} is a data frame, the 
#'   function is applied to each column given in \code{columns} (all numeric 
#'   columns by default)
#' @param breaks vector of breaks
#' @param values values to be assigned
#' @param right if TRUE the intervals are right-closed, else left-closed.
#' @param add.Inf.limits if TRUE (default), -Inf and Inf are added to the left
#'   and right, respectively, of \code{breaks}
#' @param to.factor if \code{TRUE} the new values are converted to
#'   \code{factor}. The default is \code{FALSE}.
#' @param columns \code{NULL} or vector of column names (if \code{x} is a data
#'   frame)
#' @param keyFields \code{NULL} or vector of column names (if \code{x} is a data
#'   frame). If not \code{NULL}, a data frame with these columns coming first
#'   and the interval labels in the last column is returned.
#' 
#' @export
#' 
#' @examples 
#' groupByBreaks(1:10, breaks = 5, values = c("<= 5", "> 5"))
#' groupByBreaks(1:10, breaks = 5, right = FALSE, values = c("< 5", ">= 5"))
#' 
#' # Prepare a simple data frame
#' x <- kwb.utils::noFactorDataFrame(
#'   id = c("A", "B", "C"), 
#'   value = c(10, 20, 30)
#' )
#' 
#' # Keep the ID column of the data frame
#' groupByBreaks(x, breaks = 20, keyFields = "id")
#' 
groupByBreaks <- function(
  x, breaks, values = breaksToIntervalLabels(breaks), right = TRUE, 
  add.Inf.limits = TRUE, to.factor = FALSE, columns = NULL, keyFields = NULL
)
{
  # If x is a data frame, apply this function to each given or numeric
  # column and return
  if (is.data.frame(x)) {
    
    basis <- if (! is.null(keyFields)) {
      
      kwb.utils::selectColumns(x, keyFields, drop = FALSE)
    }
    
    if (is.null(columns)) {
      
      columns <- names(which(sapply(x, is.numeric)))
    }
    
    categoricals <- lapply(
      x[columns], groupByBreaks, breaks = breaks, values = values, 
      right = right, add.Inf.limits = add.Inf.limits, to.factor = to.factor
    )
    
    categoricals <- kwb.utils::asNoFactorDataFrame(categoricals)
    
    return(cbind(basis, categoricals))
  }
  
  if (add.Inf.limits) {
    
    breaks <- c(-Inf, breaks, Inf)    
  }
  
  groups <- data.frame(
    row = seq_len(length(x)),
    groupnumber = cut(x = x, breaks = breaks, labels = FALSE, right = right)
  )
  
  ### TODO: use cut with labels = values instead of merge!
  #cut(x = x, breaks = breaks, labels = values, right = right)
  ### cut gives a warning if there are duplicated values in "labels"...
  duplicates <- values[duplicated(values)]
  
  if (length(duplicates) > 0) {
    
    message(
      "groupByBreaks: There are duplicated values (", 
      kwb.utils::stringList(duplicates), ") in the labels to be given: ", 
      kwb.utils::stringList(values),
      ".\n-> Can you modify the breaks to prevent this?"
    )
  }
  
  groupToValue <- data.frame(
    groupnumber = seq_len(length(breaks) - 1),
    values = values,
    stringsAsFactors = FALSE
  )
  
  result <- merge(groups, groupToValue, sort = FALSE, all.x = TRUE)
  
  xnew <- result$values[order(result$row)]
  
  typeConverted(xnew, to.factor, factorLevels = values)
}

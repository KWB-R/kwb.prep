# checkGrouping ----------------------------------------------------------------

#' Compare Two Columns of a Data Frame (Raw Vs Regrouped)
#'
#' @param data data frame
#' @param column_raw name of column in \code{data} containing original (raw) 
#'   values
#' @param column_cat name of column in \code{data} containing the result of
#'   regrouping the raw value into categories
#'
checkGrouping <- function(data, column_raw, column_cat)
{
  # Select only the two columns to be compared (1: raw, 2: cat)
  data <- kwb.utils::selectColumns(data, c(column_raw, column_cat))
  
  # Show the unique value combinations
  kwb.utils::printIf(
    TRUE, kwb.utils::resetRowNames(unique(data[order(data[, 1]), ])),
    caption = "Raw values and assigned categories"
  )
  
  # Show the levels of the categoised variable
  kwb.utils::printIf(
    TRUE, levels(data[, 2]), 
    caption = "\nLevels of category factor"
  )
}

# checkNumberOfUnique ----------------------------------------------------------

#' Show Number of Unique Values in Selected Columns
#' 
#' @param data data frame
#' @param columns names of columns in \code{data} for which to count unique 
#'   values
#'   
checkNumberOfUnique <- function(data, columns = names(data))
{
  cat("Number of unique values in column\n")
  
  for (column in columns) {
    
    cat(sprintf(
      "- %s: %d\n", column, length(unique(kwb.utils::selectColumns(data, column)))
    ))
  }
}

# dataFramesToTextMatrix -------------------------------------------------------

#' Convert List of Data Frames to Character Matrix
#' 
#' In the character matrix the data frames appear one below the other. Each
#' data frame has a header and each data frame is separated from the following 
#' data frame by an empty row.
#' 
#' @param data_frames list of data frames
#' 
#' @export
#' 
#' @examples 
#' data_frames <- list(
#'   data.frame(a = 1:3, b = 2:4),
#'   data.frame(a = 1:5, b = 2:6, c = 3:7)
#' )
#' 
#' dataFramesToTextMatrix(data_frames)
#' 
dataFramesToTextMatrix <- function(data_frames)
{
  stopifnot(is.list(data_frames))
  stopifnot(all(sapply(data_frames, inherits, "data.frame")))
  
  ncol <- max(sapply(data_frames, ncol))
  
  nrows <- sapply(data_frames, nrow)
  
  nrow <- sum(nrows) + 2 * length(data_frames) - 1
  
  text_matrix <- matrix("", nrow, ncol)
  
  offsets <- c(0, cumsum(nrows + 2))
  
  for (i in seq_along(data_frames)) {
    
    from_row <- 1 + offsets[i]
    to_row <- from_row + nrows[i]
    text_area <- as.matrix(data_frames[[i]])
    text_matrix[from_row, seq_len(ncol(text_area))] <- names(data_frames[[i]])
    text_matrix[(from_row + 1):to_row, seq_len(ncol(text_area))] <- text_area
  }
  
  text_matrix
}

# fillUpNA ---------------------------------------------------------------------

#' Fill NA in First Vector With Values From Second Vector
#' 
#' @param x first vector
#' @param y second vector
#' @param dbg if \code{TRUE} a debug message is shown
#' @param name_x name of x
#' @param name_y name of y
#' @return \code{x} with \code{NA} replaced by the values in \code{y} at the
#'   same positions
#' 
fillUpNA <- function(x, y, dbg = TRUE, name_x = NULL, name_y = NULL)
{
  name_x <- getname(name_x, substitute(x))
  name_y <- getname(name_y, substitute(y))
  
  stopifnot(length(x) == length(y))
  
  is_na <- is.na(x)
  
  number_of_na_before <- sum(is_na)
  
  x[is_na] <- y[is_na]
  
  kwb.utils::catIf(dbg, sprintf(
    "%d NAs in '%s' filled up with values from '%s'\n", 
    number_of_na_before - sum(is.na(x)), name_x, name_y
  ))
  
  x
}

# getChangesOfDuplicates -------------------------------------------------------

#' Get Changes of Rows That Are Duplicated in Selected Columns
#' 
#' @param df a data frame
#' @param columns names of columns in \code{df} in which to look for duplicate
#'   value combinations
#' @param add_columns names of additional columns that shall appear in the 
#'   output even if there are no changes in these columns
#'   
#' @return list of data frames. The list has as many elements as there are
#'   different value combinations in \code{columns} that appear more than once
#'   in \code{df}. Each element is a data frame with all rows from \code{df}
#'   that have the same value combination in \code{columns}. By default the data
#'   frame contains the columns given in \code{columns} and those columns out of
#'   \code{df} in which there is at least one change over the values in the
#'   different rows.
#'   
#' @export
#' 
#' @examples 
#' df <- data.frame(
#'   id = 1:7, 
#'   name = c("one", "one", "two", "two", "three", "three", "three"), 
#'   type = c("A", "A", "B", "C", "D", "D", "D"),
#'   size = c(10, 11, 12, 12, 13, 13, 14),
#'   height = c(1, 1, 2, 3, 4, 4, 5)
#' )
#' 
#' df
#' 
#' getChangesOfDuplicates(df, "name")
#' getChangesOfDuplicates(df, c("name", "type"))
#' 
getChangesOfDuplicates <- function(df, columns, add_columns = columns)
{
  fetch <- function(df, cols) kwb.utils::selectColumns(df, cols, drop = FALSE)
  
  candidates <- fetch(df, columns)
  
  groups <- unique(candidates[duplicated(candidates), , drop = FALSE])
  
  groups <- kwb.utils::fullySorted(groups)
  
  groups[[".group"]] <- seq_len(nrow(groups))
  
  y <- merge(df, groups, by = columns)
  
  if (any(is.na(y[[".group"]]))) {
    stop("Unexpected error in getChangesOfDuplicates(): .group is NA!")
  }
  
  #y <- y[! is.na(y[[".group"]]), , drop = FALSE]
  
  nm <- names(y)
  
  cols <- c(columns, add_columns)
  
  lapply(unname(split(y, y[[".group"]])), function(x) {
    fetch(x, unique(c(cols, nm[! sapply(x, kwb.utils::allAreEqual)])))
  })
}

# getYearFromColumn ------------------------------------------------------------

#' Get Integer Year Number from Column
#' 
#' @param data data frame
#' @param column representing a date or date and time
#' 
#' @return vector of integer as long as the number of rows in \code{data}
#' 
#' @export
#' 
getYearFromColumn <- function(data, column)
{
  values <- kwb.utils::selectColumns(data, column)
  
  if (inherits(values, "Date")) {
    
    as.integer(format(values, "%Y"))  
    
  } else {
    
    result <- as.integer(substr(values, 1, 4))
    
    year_range <- range(unique(stats::na.omit(result)))
    
    warning(
      "Found text when I was expecting date. Returning first four characters:", 
      year_range[1], "-", year_range[2]
    )
    
    result
  }
}

# overwriteIfNotNA -------------------------------------------------------------

#' Use Non-NA Values from Source Column in Target Column
#' 
#' Overwrite the values in the target column with the values in the source
#' column at indices where the values in the source column are not NA
#' 
#' @param data data frame
#' @param target_column name of target column
#' @param source_column name of source column
#' 
#' @export
#' 
overwriteIfNotNA <- function(data, target_column, source_column)
{
  data[[target_column]] <- replaceUnlessNA(
    x = kwb.utils::selectColumns(data, target_column), 
    substitute = kwb.utils::selectColumns(data, source_column)
  )
  
  data
}

# replaceByCondition -----------------------------------------------------------

#' Replace Values in Column in Rows Matching Condition
#' 
#' @param df data frame in which to do substitutions
#' @param file path to CSV file with columns \code{group}, 
#'   \code{target_column}, \code{condition}, \code{replacement}
#' @param group group name. If given, only rows in \code{file} that have
#'   this group name in column \code{group} are considered.
#' @param config optional. Data frame containing the configuration as being read
#'   from \code{file}.
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#' 
#' @examples 
#' # Create a very simple data frame
#' df <- data.frame(a = 1:3)
#' 
#' # Create a very simple configuration
#' config <- read.table(sep = ",", header = TRUE, text = c(
#'   "group,target_column,condition,replacement",
#'   "g1,a,a==2,22",
#'   "g2,a,a==3,33"
#' ))
#' 
#' # Write the configuration to a temporary file
#' file <- tempfile()
#' write.csv(config, file)
#' 
#' # Apply all replacements configured in the file ...
#' replaceByCondition(df, file)
#' 
#' # ... or in the configuration
#' replaceByCondition(df, config = config)
#' 
#' # Apply selected replacements
#' replaceByCondition(df, file, group = "g1")
#' replaceByCondition(df, file, group = "g2")
#' 
replaceByCondition <- function(
  df, file = NULL, group = NULL, config = NULL, dbg = TRUE
)
{
  if (is.null(file) && is.null(config)) {
    stop_("Either 'file' or 'config' must be given.")
  }
  
  if (is.null(config)) {
    config <- utils::read.csv(file, stringsAsFactors = FALSE)
  }

  fetch <- kwb.utils::createAccessor(config)
  
  if (! is.null(group)) {
    
    groups <- fetch("group")
    stopIfNotIn(group, unique(groups), singular = "group")
    config <- config[groups == group, , drop = FALSE]
    fetch <- kwb.utils::createAccessor(config)
  }

  #stopifnot(! anyDuplicated(fetch("target_column")))
  
  # Evaluate all criteria at once
  matches <- kwb.utils::getAttribute("details", x = kwb.utils::matchesCriteria(
    Data = df, 
    criteria = as.character(fetch("condition")), 
    na.to.false = TRUE, 
    add.details = TRUE, 
    dbg = FALSE
  ))

  # Provide column vectors of "config"
  columns <- as.character(fetch("target_column"))
  replacements <- fetch("replacement")
  
  # Apply the replacements  
  for (i in seq_along(columns)) {
    df[which(matches[, i]), columns[i]]<- replacements[i]
  }

  # Provide metadata on the results of the applied replacements  
  metadata <- kwb.utils::resetRowNames(cbind(
    kwb.utils::removeColumns(config, "group"), 
    n_replaced = colSums(matches, na.rm = TRUE)
  ))

  # Print the metadata
  kwb.utils::catIf(dbg, paste(collapse = "", sprintf(
    "Data correction in '%s': %d values with \"%s\" set to '%s'\n",
    metadata$target_column,
    metadata$n_replaced, 
    metadata$condition, 
    metadata$replacement
  )))
  
  structure(df, metadata = metadata)
}

# replaceUnlessNA ------------------------------------------------------------

#' Use Elements of Substitute at Indices Where Substitutes Are Not NA
#' 
#' @param x vector in which to substitute
#' @param substitute vector containing substitutions
#' 
replaceUnlessNA <- function(x, substitute)
{
  stopifnot(length(x) == length(substitute))
  
  ok <- ! is.na(substitute)
  
  x[ok] <- substitute[ok]
  
  x
}

# reportNA ---------------------------------------------------------------------

#' Count NA in a Column and Give a Message about It
#' 
#' @param data data frame
#' @param column name of column in \code{data}
#' @param subject value for placeholder \emph{subject} in output: "NAs 
#'   \emph{subject}: \emph{count_NA}"
#' 
reportNA <- function(data, column, subject = "in data")
{
  number_of_na <- kwb.utils::countNaInColumn(data, column)
  
  cat(sprintf("NAs %s: %d\n", subject, number_of_na))
  
  number_of_na
}

# stopIfNotIn ------------------------------------------------------------------

#' Stop with Info If Element Is Not in Expected Set
#' 
#' Stop with info message if element is not in expected set of elements
#' 
#' @param element element to be looked for in \code{elements}
#' @param elements vector of possible elements
#' @param singular name of object to appear in error message. Default: 
#'   \code{"option"}
#' @param plural name of object (plural) to appear in error message. Default: 
#'   \code{paste0(singular, "s")}
#' @export
#' 
stopIfNotIn <- function(
  element, elements, singular = "option", plural = paste0(singular, "s")
)
{
  if (! element %in% elements) {
    
    stopf(
      "No such %s: '%s'. Available %s: %s",
      singular, element, plural, kwb.utils::stringList(elements)
    )
  }
}

# stopOnDuplicates -------------------------------------------------------------

#' Stop If There Are Duplicates over given Columns
#' 
#' @param data data frame
#' @param columns names of columns over which to look for duplicates. By 
#'   default, all columns in \code{data} are used.
#' @param name name of data 
#' @export
#' 
stopOnDuplicates <- function(data, columns = names(data), name = NULL)
{
  name <- getname(name, substitute(data))
  
  changes <- getChangesOfDuplicates(data, columns)
  
  if (length(changes) > 0) {
    
    print(changes)
    
    stopf(
      "There are duplicates in column(s) %s of %s (see above)!", 
      kwb.utils::stringList(columns), name
    )
  }
}

# writeStandardCsv -------------------------------------------------------------

#' Write CSV File in a Standardised Manner
#' 
#' @param x data frame
#' @param file path to CSV file to be written
#' @param \dots additional arguments passed to \code{\link[utils]{write.table}}
#' 
#' @export
#' 
writeStandardCsv <- function(x, file, ...)
{
  utils::write.table(
    x, file = file, col.names = TRUE, row.names = FALSE, dec = ".", sep = ";", 
    quote = FALSE, ...
  )
}

# removeRowsThatAreNaInColumn --------------------------------------------------

#' Remove Rows That are NA in Given Column
#' 
#' @param data data frame
#' @param column column name
#' @param dbg it \code{TRUE} debug messages are shown
#' 
#' @return \code{data} with rows removed that are \code{NA} in
#'   \code{data[[column]]}
#'   
#' @export
#' 
#' @examples 
#' df <- data.frame(a = c(1, NA, 3), b = c(11, 22, NA))
#' df
#' 
#' removeRowsThatAreNaInColumn(df, "a")
#' removeRowsThatAreNaInColumn(df, "b")
#' 
removeRowsThatAreNaInColumn <- function(data, column, dbg = TRUE)
{
  is_na <- is.na(kwb.utils::selectColumns(data, column))
  
  kwb.utils::catAndRun(
    dbg = dbg,
    sprintf("Removing %d rows that are NA in column '%s'", sum(is_na), column),
    data[! is_na, ]
  )
}

# printNumberOfNA --------------------------------------------------------------

#' Print Number of NA Values in Given Column
#' 
#' @param data data frame
#' @param column column name
#' @param name name of data
#' @export
#' 
printNumberOfNA <- function(data, column, name = NULL)
{
  name <- getname(name, substitute(data))
  
  cat(sprintf("\nis.na(%s$%s):\n", name, column))
  
  print(table(is.na(kwb.utils::selectColumns(data, column))))
}

# printTableForColumn ----------------------------------------------------------

#' Print Result of table() for Given Column
#' 
#' @param data data frame
#' @param column column name
#' @param name name of data
#' @export
#' 
printTableForColumn <- function(data, column, name = NULL)
{
  name <- getname(name, substitute(data))
  
  cat(sprintf("\ntable(%s$%s):\n", name, column))
  
  print(table(kwb.utils::selectColumns(data, column), useNA = "always"))
}

# logicalToYesNo ---------------------------------------------------------------

#' Convert Vector of Logical to Vector of "Ja"/"Nein"
#' 
#' @param x vector of logical
#' @param yesno vector of character of length two giving the strings to be used
#'   for \code{TRUE} and \code{FALSE}, respectively
#'   
#' @return vector of character
#' 
#' @export
#' 
#' @examples 
#' logicalToYesNo(c(TRUE, FALSE, TRUE))
#' logicalToYesNo(c(TRUE, FALSE, TRUE), yesno = c("Yeah!", "Oh no!"))
#' 
logicalToYesNo <- function(x, yesno = c("Ja", "Nein"))
{
  ifelse(x, yesno[1], yesno[2])
}

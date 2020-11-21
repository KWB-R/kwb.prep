# find_identical_columns -------------------------------------------------------
find_identical_columns <- function(a, b)
{
  record <- function(a, b) {
    length.out <- max(length(a), length(b))
    xt <- function(x) kwb.utils::enlargeVector(x, length.out)
    kwb.utils::noFactorDataFrame(column_a = xt(a), column_b = xt(b))
  }
  
  seen <- rep(FALSE, length(a))

  result <- lapply(unname(a), function(x) {

    in_a <- sapply(a, identical, x) & ! seen
    in_b <- sapply(b, identical, x)
  
    seen[in_a] <<- TRUE

    if (! any(in_a) || ! any(in_b)) {
      record(character(), character())
    } else {
      record(names_which(in_a), names_which(in_b))
    }
  })

  kwb.utils::moveColumnsToFront(columns = "group", kwb.utils::rbindAll(
    result, nameColumn = "group"
  ))
}

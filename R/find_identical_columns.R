# find_identical_columns -------------------------------------------------------
find_identical_columns <- function(x, y)
{
  record <- function(x, y) {
    n <- max(length(x), length(y))
    xt <- function(z) kwb.utils::enlargeVector(z, length.out = n)
    kwb.utils::noFactorDataFrame(column_x = xt(x), column_y = xt(y))
  }
  
  seen <- rep(FALSE, length(x))

  result <- lapply(unname(x), function(z) {

    in_x <- sapply(x, identical, z) & ! seen
    in_y <- sapply(y, identical, z)
  
    seen[in_x] <<- TRUE

    if (! any(in_x) || ! any(in_y)) {
      record(character(), character())
    } else {
      record(names_which(in_x), names_which(in_y))
    }
  })

  kwb.utils::rbindAll(result, nameColumn = "group")
}

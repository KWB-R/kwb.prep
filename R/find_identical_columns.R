# find_identical_columns -------------------------------------------------------
find_identical_columns <- function(a, b)
{
  names_a <- names(a)
  names_b <- names(b)

  seen <- rep(FALSE, length(a))

  identicals <- lapply(unname(a), function(x) {

    in_a <- sapply(a, identical, x) & ! seen
    in_b <- sapply(b, identical, x)

    seen[in_a] <<- TRUE

    if (! any(in_a) || ! any(in_b)) {
      return()
    }

    length.out <- max(sum(in_a), sum(in_b))
    
    enlarge <- function(x) kwb.utils::enlargeVector(x, length.out)

    kwb.utils::noFactorDataFrame(
      column_a = enlarge(names_a[in_a]),
      column_b = enlarge(names_b[in_b])
    )
  })

  kwb.utils::excludeNULL(identicals, dbg = FALSE) %>%
    kwb.utils::rbindAll(nameColumn = "group") %>%
    kwb.utils::moveColumnsToFront("group")
}

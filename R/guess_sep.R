# guess_sep --------------------------------------------------------------------
guess_sep <- function(file, candidates = c(";", ","))
{
  header <- readLines(file, 1L)

  fields <- lapply(candidates, function(x) strsplit(header, x)[[1L]])

  index <- which.max(lengths(fields))

  sep <- candidates[index]

  if (length(fields[[index]]) <= 1L) {
    message(sprintf(
      "I found only one column (%s) in %s. Is '%s' the correct column separator?",
      fields[[index]][1L], basename(file), sep
    ))
  }

  sep[1L]
}

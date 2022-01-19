# compare_ids ------------------------------------------------------------------
compare_ids <- function(old, new) {

  common <- intersect(old, new)

  added <- setdiff(new, common)
  removed <- setdiff(old, common)

  stopifnot(identical(added, new[! new %in% old]))
  stopifnot(identical(removed, old[! old %in% new]))

  result <- list(
    n_old = length(old),
    n_new = length(new),
    n_added = length(added),
    n_removed = length(removed),
    n_common = length(common),
    old = old,
    new = new,
    added = added, 
    removed = removed, 
    common = common
  )
  
  utils::str(result)
  
  result
}

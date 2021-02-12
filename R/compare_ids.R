# compare_ids ------------------------------------------------------------------
compare_ids <- function(old_ids, new_ids) {

  common_ids <- intersect(old_ids, new_ids)

  added_ids <- setdiff(new_ids, common_ids)
  removed_ids <- setdiff(old_ids, common_ids)

  stopifnot(identical(added_ids, new_ids[! new_ids %in% old_ids]))
  stopifnot(identical(removed_ids, old_ids[! old_ids %in% new_ids]))

  kwb.utils::printIf(TRUE, length(old_ids))
  kwb.utils::printIf(TRUE, length(new_ids))
  kwb.utils::printIf(TRUE, length(common_ids))
  kwb.utils::printIf(TRUE, length(removed_ids))
  kwb.utils::printIf(TRUE, length(added_ids))

  kwb.utils::printIf(TRUE, utils::head(sort(removed_ids), 20L))
  kwb.utils::printIf(TRUE, utils::head(sort(added_ids), 20L))

  list(
    added_ids = added_ids,
    removed_ids = removed_ids,
    common_ids = common_ids
  )
}

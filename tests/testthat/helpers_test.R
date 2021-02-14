is_character_list <- function(x) {
  is.list(x) && all(sapply(x, class) == "character")
}

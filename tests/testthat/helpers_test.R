package_config_dir <- system.file("extdata", "config", package = "kwb.prep")

kwb.prep:::set_user_config_dir(package_config_dir)

is_character_list <- function(x) {
  is.list(x) && all(sapply(x, class) == "character")
}

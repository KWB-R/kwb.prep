# Make sure that error messages appear in German (TODO: better English?)
Sys.setenv(LANGUAGE = "DE")

# Set the configuration directory to extdata/config in this (installed) package 
set_user_config_dir(extdata_file("config", dbg = FALSE))

is_character_list <- function(x) {
  is.list(x) && all(sapply(x, class) == "character")
}

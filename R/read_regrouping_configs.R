# read_regrouping_configs ------------------------------------------------------
read_regrouping_configs <- function(config_dir)
{
  # Helper function to read all csv files in a directory
  read_all_csv <- function(path) {
    files <- dir(path, "\\.csv$", full.names = TRUE)
    lapply(
      stats::setNames(files, kwb.utils::removeExtension(basename(files))),
      utils::read.table, 
      sep = ";", 
      header = TRUE, 
      stringsAsFactors = FALSE
    )
  }
  
  # Read configurations from csv files
  dfs <- read_all_csv(kwb.utils::safePath(config_dir, "regrouping"))

  # Restructure configurations to lists
  lapply(dfs, function(x) {
    if ("breaks" %in% names(x)) {
      x <- as.list(x)
      x$breaks <- x$breaks[- length(x$breaks)]
      x$right <- x$right[1L]
      x[kwb.utils::moveToFront(names(x), c("right", "breaks"))]
    } else {
      as.list(x)
    }
  })
}

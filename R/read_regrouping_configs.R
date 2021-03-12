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
  dfs_num <- read_all_csv(file.path(config_dir, "regrouping_num"))
  dfs_cat <- read_all_csv(file.path(config_dir, "regrouping_cat"))
  
  # Restructure configurations to list
  configs_cat <- lapply(dfs_cat, as.list)
  configs_num <- lapply(dfs_num, function(x) {
    #x <- dfs_num[[1]]
    x <- as.list(x)
    x$breaks <- x$breaks[- length(x$breaks)]
    x$right <- x$right[1L]
    x[kwb.utils::moveToFront(names(x), c("right", "breaks"))]
  })
  
  c(configs_cat, configs_num)
}

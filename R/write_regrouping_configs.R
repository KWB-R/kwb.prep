# write_regrouping_configs -----------------------------------------------------
write_regrouping_configs <- function(config, actual_df, target_dir)
{
  # Helper function to create a subdirectory in the config directory
  create_dir <- function(x) kwb.utils::createDirectory(file.path(target_dir, x))
  
  # Helper function to get the names of elements that contain a certain element
  elements_with <- function(x) {
    names(kwb.utils::excludeNULL(lapply(config, "[[", x)))
  }
  
  # Helper function to convert partial configs to data frames
  to_df <- function(x) configs_to_data_frames(configs = config[x], actual_df)
  
  # Identify numeric partial configs, convert to data frames and write to csv 
  write_data_frames(
    to_df(elements_with("breaks")), 
    path = create_dir("regrouping_num")
  )
  
  # Identify categorial partial configs, convert to data frames and write to csv 
  write_data_frames(
    to_df(elements_with("values")), 
    path = create_dir("regrouping_cat")
  )
}

# configs_to_data_frames -------------------------------------------------------
configs_to_data_frames <- function(configs, actual_df)
{
  result <- lapply(stats::setNames(nm = names(configs)), function(element) {
    
    #element <- names(configs)[1]
    #element <- "Age_cat"
    print(element)
    
    # Keep only labels that are acutally used
    if (! any(is_used <- actual_df$name == element)) {
      return(NULL)
    }
    
    x <- configs[[element]]
    
    y <- if (is_num <- ! is.null(x$breaks)) {
      x[setdiff(names(x), c("breaks", "right"))]
    } else {
      x
    }
    
    label_keys <- actual_df$labels[is_used]
    
    if (! is_num) {
      return(kwb.utils::asNoFactorDataFrame(y[c("values", label_keys)]))
    }
    
    result <- kwb.utils::asNoFactorDataFrame(y[label_keys])
    
    enlarge_element <- function(element) kwb.utils::enlargeVector(
      kwb.utils::selectElements(x, element), 
      nrow(result)
    )
    
    cbind(
      result, 
      breaks = enlarge_element("breaks"),
      right = enlarge_element("right"),
      stringsAsFactors = FALSE
    )
  })
  
  kwb.utils::excludeNULL(result)
}

# write_data_frames ------------------------------------------------------------
write_data_frames <- function(dfs, path)
{
  lapply(names(dfs), function(name) {
    file <- file.path(path, paste0(name, ".csv"))
    utils::write.table(dfs[[name]], file, sep = ";", row.names = FALSE)
    file
  })
}

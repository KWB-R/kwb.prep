# Define argument sets for remove_columns()
if (FALSE)
{
  configs <- list(
    duplicated_in_cctv = list(
      columns = c("n_rehab", "n_repairs", "ConstructionDate_raw")
    ),
    intermediate = list(
      pattern = "((Ground|Invert)Level(Up|Down)stream)|LENGTH_FROM_COORDS",
      reason = "reason_intermediate"
    ),
    intermediate_lookup = list(
      pattern = "Insp$",
      reason = "reason_intermediate_lookup"
    ),
    this_year = list(
      columns = "THIS_YEAR",
      reason = "reason_this_year"
    )
  )
}

# Write and check yaml file ----------------------------------------------------
if (FALSE)
{
  file <- kwb.prep:::config_file("args_remove_columns.yml", must_exist = FALSE)
  yaml::write_yaml(configs, file)
  
  stopifnot(identical(
    kwb.prep:::read_args("remove_columns", dbg = TRUE), 
    configs
  ))
}

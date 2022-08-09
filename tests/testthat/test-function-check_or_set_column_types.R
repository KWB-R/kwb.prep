test_that("check_or_set_column_types() works", {

  f <- kwb.prep:::check_or_set_column_types

  expect_error(f())
  
  field_config <- list(
    height = list(type = "a"),
    diametre = list(type = "b"),
    age = list(type = "c")
  )
  
  trees <- data.frame(
    height = c(10.0, 20.0, 30.0), 
    age = c(10L, 20L, 30L)
  )

  type_info <- c(
    "internal_field;internal_type",
    "height;integer",
    "age;character",
    "width;character"
  )
  
  prep_options <- list(
    column_separator = ";",
    mode = "production"
  )
  
  #str(trees)
  old_config_dir <- get_user_config_dir()
  
  tmp_config_dir <- kwb.utils::tempSubdirectory("test", "config")
  
  writeLines(type_info, file.path(tmp_config_dir, "internal-types.csv"))
  yaml::write_yaml(prep_options, file.path(tmp_config_dir, "options.yml"))
  
  set_user_config_dir(tmp_config_dir)
  result <- f(trees, field_config, dbg = FALSE)

  set_user_config_dir(old_config_dir)
})

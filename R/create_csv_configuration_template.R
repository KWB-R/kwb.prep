# create_csv_configuration_template --------------------------------------------
create_csv_configuration_template <- function(
  path_db, pattern = "\\.csv$", nrows = 5L
)
{
  files <- dir(path_db, pattern, full.names = TRUE)

  # Create the csv configuration
  default_config <- list(
    default = list(
      sep = ";"
    )
  )

  main_config <- lapply(files, function(file) {

    sep <- guess_sep(file)

    data <- utils::read.table(
      file,
      sep = sep,
      nrows = nrows,
      header = TRUE,
      stringsAsFactors = FALSE
    )

    fields <- lapply(names(data), function(name) {
      list(
        field = name,
        type = class(data[[name]])
      )
    })

    names(fields) <- tolower(names(data))

    list(
      file = basename(file),
      sep = sep,
      fields = fields
    )
  })

  names(main_config) <- tolower(kwb.utils::removeExtension(basename(files)))

  c(default_config, main_config)
}

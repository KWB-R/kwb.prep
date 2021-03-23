# render_text ------------------------------------------------------------------
render_text <- function(
  rmd_source,
  params_to_rmd,
  rmd_file = tempfile(fileext = ".Rmd"),
  show = TRUE
)
{
  writeLines(rmd_source, con = rmd_file)

  kwb.utils::catAndRun(sprintf("Rendering '%s'", rmd_file), {

    html_file <- rmarkdown::render(
      rmd_file,
      params = params_to_rmd,
      quiet = TRUE
    )
  })

  if (isTRUE(show)) {
    utils::browseURL(html_file)
  }

  html_file
}

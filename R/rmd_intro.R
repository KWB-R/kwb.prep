# rmd_intro --------------------------------------------------------------------
rmd_intro <- function(title, author = "Hauke Sonnenberg")
{
  double_quoted <- function(x) kwb.utils::hsQuoteChr(x)

  c(
    "---",
    paste("title:", double_quoted(title)),
    paste("author:", double_quoted(author)),
    paste("date:", double_quoted(Sys.Date())),
    paste("output:", "html_document"),
    "params:",
    "  script_info: NULL",
    "  all_function_info: NULL",
    "---\n"
  )
}

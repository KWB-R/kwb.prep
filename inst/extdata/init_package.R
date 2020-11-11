package <- "kwb.prep"

author <- list(
  name = "Hauke Sonnenberg",
  orcid = "0000-0001-9134-2871",
  url = "https://github.com/hsonne"
)

description <- list(
  name = package,
  title = "Markdown-Documented Data Preparation",
  desc  = "R Package for Markdown-documented data preparation."
)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

kwb.utils::createDirectory("inst/extdata")

test_that("get_renamings_from_config() works", {

  f <- kwb.prep:::get_renamings_from_config

  #source("tests/testthat/helpers_test.R")

  expect_error(f())

  pipes_1 <- list(
    file = "DatenCO_Haltungen_utf8.csv",
    fields = list()
  )

  pipes_2 <- list(
    file = "DatenCO_Haltungen_utf8.csv",
    fields = list(
      a = list(field = "my_a"),
      b = list(field = "my_b")
    )
  )

  config_0 <- list(default = list(sep = ","))

  config_1 <- config_0
  config_1$pipes <- pipes_1

  config_2 <- config_0
  config_2$pipes <- pipes_2

  config_3 <- config_2
  config_3$inspections <- list(
    fields = list(
      c = list(field = "my_c"),
      d = list(field = "my_d")
    )
  )

  expect_identical(f(config_1, table_name = "pipes"), list())
  expect_identical(f(config_1, table_name = "pipes", all = FALSE), list())

  expect_identical(f(config_2, "pipes"), list(my_a = "a", my_b = "b"))

  expect_error(
    f(config_3, c("inspections", "pipes")),
    "character of length one"
  )
})

#library(testthat)
test_that("replaceByCondition() works", {

  f <- kwb.prep:::replaceByCondition
  
  expect_error(f())

  file <- tempfile("replace-by-condition_", fileext = ".csv")
  
  writeLines(con = file, c(
    "# Comment (to be ignored)",
    "group,target,condition,replacement",
    "# Comment (to be ignored)",
    "g1,i,i < 3,NA",
    "g1,i,j == 4,44",
    "g2,j,i <= 2,NA"
  ))
  
  #writeLines(readLines(file))
  #x <- read.csv(file, comment.char = "#")
  #str(x)
  
  df <- data.frame(i = 1:4, j = 2:5)
  
  result_1 <- f(df, file, group = "g1", dbg = FALSE)
  result_2 <- f(df, file, group = "g2", dbg = FALSE)
  
  expect_identical(result_1$i, c(NA, NA, 44L, 4L))
  expect_identical(result_2$j, c(NA, NA, 4L, 5L))
})

test_that("set_dbg() works", {

  dbg <- 4L
  
  kwb.prep:::set_dbg(dbg)
  
  expect_true(kwb.prep:::get_dbg() == dbg)
})

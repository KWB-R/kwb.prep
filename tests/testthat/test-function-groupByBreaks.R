#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2021-03-06 08:51:12.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("groupByBreaks() works", {

  f <- kwb.prep:::groupByBreaks

  expect_error(
    kwb.prep:::groupByBreaks()
    # Argument "x" fehlt (ohne Standardwert)
  )

})

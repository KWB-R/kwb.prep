#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2021-03-06 08:51:12.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("toAssignments2() works", {

  f <- kwb.prep:::toAssignments2

  expect_error(
    f()
    # Argument "labels" fehlt (ohne Standardwert)
  )

})

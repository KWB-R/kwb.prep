#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hsonne on 2021-02-14 04:32:55.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("kable_data_frame_structure() works", {

  expect_error(
    kwb.prep:::kable_data_frame_structure()
    # Argument "df" fehlt (ohne Standardwert)
  )

})


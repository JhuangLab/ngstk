test_that("Time stamp", {
  x <- time_stamp()
  expect_that(is.list(x), equals(TRUE))
  expect_that(length(x), equals(4))
})

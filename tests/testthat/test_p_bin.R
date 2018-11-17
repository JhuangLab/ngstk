test_that("rbin", {
  status <- rbin("ngstk", tempdir())
  status <- as.data.frame(status)
  expect_that(status["ngstk.demo_bin.sh", 1], equals(TRUE))
})

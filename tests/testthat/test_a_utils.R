test_that("set_tools", {
  config.file <- system.file("extdata", "demo/tools_config.json", package = "ngstk")
  config.list <- list(gatk = "/path/gatk")
  config.vec <- c("/path/samtools")
  names(config.vec) <- "samtools"
  tools <- set_tools(config.file, config.list, config.vec, 
                     eval.params = list(config = "tools"))
  expect_that(is.list(tools), equals(TRUE))
  expect_that(length(tools), equals(3))
  expect_that(names(tools)[1], equals("gmap"))
})
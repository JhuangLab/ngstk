test_that("muts2pp", {
  demo_file <- system.file("extdata", "demo/proteinpaint/muts2pp_iseq.txt", package = "ngstk")
  input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  disease <- "T-ALL"
  input_data <- data.frame(input_data, disease)
  input_data$disease <- as.character(input_data$disease)
  result <- muts2pp(input_data, input_type = "iseq")
  result_colnames <- colnames(result)
  colnames_len <- length(result_colnames)
  expect_that(colnames_len, equals(8))
  expect_that(result_colnames[1], equals("gene"))
  expect_that(result_colnames[3], equals("chromosome"))
  expect_that(result_colnames[6], equals("class"))
  expect_that(result_colnames[8], equals("sample"))
})


test_that("fusions2pp", {
  demo_file <- system.file("extdata", "demo/proteinpaint/fusions2pp_fusioncatcher.txt", package = "ngstk")
  input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  disease <- "B-ALL"
  sampletype <- "diagnose"
  input_data <- data.frame(input_data, disease, sampletype)
  input_data$disease <- as.character(input_data$disease)
  result <- fusions2pp(input_data, input_type = "fusioncatcher")
  result_colnames <- colnames(result)
  colnames_len <- length(result_colnames)
  expect_that(colnames_len, equals(13))
  expect_that(result_colnames[1], equals("disease"))
  expect_that(result_colnames[3], equals("gene_a"))
  expect_that(result_colnames[6], equals("position_a"))
  expect_that(result_colnames[13], equals("patient"))
  expect_that(result[1, 3], equals("TCF3"))
  expect_that(result[1, 7], equals("PBX1"))
  expect_that(result[7, 5], equals("chr4"))
  expect_that(result[7, 6], equals("144801564"))
  expect_that(result[7, 12], equals("-"))
})


test_that("fusions2pp_meta", {
  demo_file <- system.file("extdata", "demo/proteinpaint/fusions2pp_fusioncatcher.txt", package = "ngstk")
  input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  disease <- "B-ALL"
  sampletype <- "diagnose"
  input_data <- data.frame(input_data, disease, sampletype)
  input_data$disease <- as.character(input_data$disease)
  result <- suppressWarnings(fusions2pp_meta(input_data, input_type = "fusioncatcher"))
})

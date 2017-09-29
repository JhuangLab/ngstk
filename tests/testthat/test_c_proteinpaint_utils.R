test_that("get_pp_samplegroup", {
  samples <- sprintf("A%s", 1:7)
  group <- "B-ALL"
  samplegroup <- get_pp_samplegroup(samples, group)
  expect_that(colnames(samplegroup)[1], equals("group"))
  expect_that(nrow(samplegroup), equals(7))
  outfn <- tempfile()
  samplegroup <- get_pp_samplegroup(samples, group, outfn = outfn)
  expect_that(file.size(outfn) > 0, equals(TRUE))
})


test_that("format_pp_meta_age", {
  meta_template <- system.file("extdata", "demo/proteinpaint/heatmap_meta_template.txt", package = "ngstk")
  raw_meta <- read.table(meta_template, sep = "\t", header = TRUE)
  term <- group <- "Age"
  raw_meta$term <- term
  raw_meta$group <- group
  raw_meta$value <- c(rep(c("Adult", "Pediatric"), 3), "Male")
  meta_age <- format_pp_meta_age(raw_meta)
  expect_that(meta_age$color[1], equals("#c20b01"))
  expect_that(meta_age$color[2], equals("#196abd"))
})

test_that("format_pp_meta_gender", {
  meta_template <- system.file("extdata", "demo/proteinpaint/heatmap_meta_template.txt", package = "ngstk")
  raw_meta <- read.table(meta_template, sep = "\t", header = TRUE)
  term <- group <- "Gender"
  raw_meta$term <- term
  raw_meta$group <- group
  raw_meta$value <- c(rep(c("Male", "Female"), 3), "Male")
  meta_gender <- format_pp_meta_gender(raw_meta)
  expect_that(meta_gender$color[1], equals("#c20b01"))
  expect_that(meta_gender$color[2], equals("#196abd"))
})

test_that("format_pp_meta_fusions", {
  meta_template <- system.file("extdata", "demo/proteinpaint/heatmap_meta_template.txt", package = "ngstk")
  raw_meta <- read.table(meta_template, sep = "\t", header = TRUE)
  meta_test_1 <- raw_meta
  term <- group <- "Fusions"
  meta_test_1$term <- term
  meta_test_1$group <- group
  meta_test_1$value <- c(rep(c("ZNF384-Fusions", "MEF2D-Fusions"), 3), "TCF3-PBX1")
  meta_fusions <- format_pp_meta_fusions(meta_test_1)
  expect_that(meta_fusions$color[1], equals("#0073c3"))
  expect_that(meta_fusions$color[2], equals("#efc000"))
  expect_that(meta_fusions$color[7], equals("#696969"))
  meta_test_2 <- raw_meta
  term <- group <- c(rep(c("MEF2D-Fusions", "ZNF384-Fusions"), 3), "DUX4-Fusions")
  meta_test_2$term <- term
  meta_test_2$group <- group
  meta_test_2$value <- c("MEF2D-PA", "EP300-ZNF384", "MEF2D-PB", "ABC-ZNF384", "MEF2D-PB", "ABD-ZNF384", 
    "DUX4-IGH")
  meta_fusions <- format_pp_meta_fusions(meta_test_2)
  expect_that(meta_fusions$color[1], equals("#0073c3"))
  expect_that(meta_fusions$color[2], equals("#0073c3"))
  expect_that(meta_fusions$color[3], equals("#efc000"))
  expect_that(meta_fusions$color[4], equals("#efc000"))
  expect_that(meta_fusions$color[6], equals("#696969"))
  expect_that(meta_fusions$color[7], equals("#0073c3"))
})


test_that("fusions_filter", {
  demo_file <- system.file("extdata", "demo/proteinpaint/fusions2pp_fusioncatcher.txt", package = "ngstk")
  input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  
  mhander_extra_params = list(gene_5 = 1, gene_3 = 2, any_gene = "TCF3", fusions_any_match_flag = TRUE)
  result_1 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
  
  mhander_extra_params = list(gene_3 = 2, right_gene = "GYPA", fusions_right_match_flag = TRUE)
  result_2 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
  
  mhander_extra_params = list(gene_5 = 1, left_gene = "GYPA", fusions_left_match_flag = TRUE)
  result_3 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
  
  mhander_extra_params = list(gene_5 = 1, gene_3 = 2, left_gene = "GYPE", right_gene = "GYPA", fusions_full_match_flag = TRUE)
  result_4 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
  
  mhander_extra_params = list(gene_5 = 1, gene_3 = 2, left_gene = "GYPE", right_gene = "GYPA", fusions_anyfull_match_flag = TRUE)
  result_5 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
  
  expect_that(nrow(result_1), equals(5))
  expect_that(nrow(result_2), equals(2))
  expect_that(nrow(result_3), equals(0))
  expect_that(nrow(result_4), equals(2))
  expect_that(result_1[1, 1], equals("TCF3"))
  expect_that(result_1[1, 2], equals("PBX1"))
  expect_that(result_2[1, 2], equals("GYPA"))
  expect_that(result_4[1, 1], equals("GYPE"))
})

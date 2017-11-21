test_that("merge_table_files", {
  a <- data.frame(col1=1:6, col2=2:7)
  b <- data.frame(col1=6:11, col2=1:6)
  file_a <- paste0(tempfile(), ".a_temp")
  file_b <- paste0(tempfile(), ".a_temp")
  write.table(a, file_a, sep = "\t", row.names = FALSE)
  write.table(b, file_b, sep = "\t", row.names = FALSE)
  input_files <- c(file_a, file_b)
  x <- merge_table_files(input_files = input_files)
  expect_that(ncol(x), equals(3))
  expect_that(colnames(x)[2], equals("col1"))
  expect_that(colnames(x)[3], equals("col2"))
  
  x <- merge_table_files(input_files = input_files, add.filename = FALSE)
  expect_that(ncol(x), equals(2))
  expect_that(colnames(x)[1], equals("col1"))
  expect_that(colnames(x)[2], equals("col2"))
  
  outfn = paste0(tempfile())
  x <- merge_table_files(files_dir = tempdir(), pattern = ".*.a_temp$", outfn = outfn)
  expect_that(is.null(x), equals(TRUE))
  x <- read.table(outfn, header = TRUE, sep = "\t")
  expect_that(ncol(x), equals(3))
  expect_that(colnames(x)[2], equals("col1"))
  expect_that(colnames(x)[3], equals("col2"))
})



test_that("par_download test", {
 urls <- c(paste0("https://raw.githubusercontent.com/",
  "Miachol/ftp/master/files/images/bioinstaller/maftools3.png"),
   paste0("https://raw.githubusercontent.com/",
   "Miachol/ftp/master/files/images/bioinstaller/maftools4.png"))
 x <- par_download(urls, sprintf("%s/%s", tempdir(), basename(urls)))
 expect_that(x[[1]], equals(0))
 expect_that(x[[2]], equals(0))
 sapply(names(x), function(x) {
   outlog <- sprintf("%s/logs/%s.log",
                   tempdir(), x)
   expect_that(file.exists(outlog), equals(TRUE))
   expect_that(file.size(outlog) > 0, equals(TRUE))
 })
})

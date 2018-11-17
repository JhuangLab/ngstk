test_that("cli", {
  option_list <- list(
    make_option(c("-l", "--list-all-subcmds"), action = "store_true", default = FALSE, help = "Print all supported subcmds of ngsjs.")
  ) 
  subcmds_list <- list(subcmd1 = "Use method 1 to plot boxplot", 
                       subcmd2 = "Use method 2 to plot boxplot")
  description <- "Method to plot boxplot"
  usage <- "usage: %prog [options] [params]"
  opt_parser_obj <- opt_parser(subcmds_list = subcmds_list, 
                               option_list = option_list,
                               description = description,
                               usage = usage)
  x <- print_help(opt_parser_obj)
  expect_that(is.null(x), equals(TRUE))
})

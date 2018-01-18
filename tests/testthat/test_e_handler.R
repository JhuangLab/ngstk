test_that("show_handler_lib", {
  suppressMessages(show_handlers(show_description = TRUE))
  suppressMessages(show_mhandlers(show_description = TRUE))
  
  suppressMessages(show_handlers(show_description = FALSE, show_all_funs = F, show_code = "handler_na_replace"))
  x <- suppressMessages(show_mhandlers(show_description = FALSE, show_all_funs = F, 
    show_code = "mhandler_fusions_anyfull_match"))
  expect_equal(is.null(x), TRUE)
})

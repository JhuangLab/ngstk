test_that("show_hander_lib", {
  suppressMessages(show_handers(show_description = TRUE))
  suppressMessages(show_mhanders(show_description = TRUE))
  
  show_handers(show_description = FALSE, show_all_funs = F, show_code = "handler_na_replace")
  show_mhanders(show_description = FALSE, show_all_funs = F, show_code = "mhander_fusions_anyfull_match")
})

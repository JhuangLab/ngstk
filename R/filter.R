#' Fusions handler_data filter that can be used to prepare the input data for downstream analysis
#' 
#' @param input_data A data frame containing the fusions cols (gene5, gene3, fusion_type)
#' @param input_type Fusion filter type
#' @param config_file ngstk filter configuration file path, default is 
#' system.file('extdata', 'config/filter.toml', package = 'ngstk')
#' @param config_list ngstk filter configuration, default is NULL and 
#' read from config_file
#' @param handler_confg_file ngstk handler configuration file path, default is 
#' system.file('extdata', 'config/handler.toml', package = 'ngstk')
#' @param mhandler_confg_file ngstk handler configuration file path, default is 
#' system.file('extdata', 'config/mhandler.toml', package = 'ngstk')
#' @param handler_funs handler function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhandler_funs handler function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param handler_extra_params Extra parameters pass to handler
#' @param mhandler_extra_params Extra parameters pass to mhandler
#' system.file('extdata', 'config/filter.toml', package = 'ngstk')
#' @param outfn Default is NULL and not output the result to file
#' @export
#' @return 
#' A data frame
#' @examples 
#' demo_file <- system.file('extdata', 
#' 'demo/proteinpaint/fusions2pp_fusioncatcher.txt', package = 'ngstk')
#' input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#' result <- fusions_filter(input_data)
fusions_filter <- function(input_data, input_type = "common", config_file = system.file("extdata", 
  "config/filter.toml", package = "ngstk"), config_list = NULL, handler_confg_file = system.file("extdata", 
  "config/handler.toml", package = "ngstk"), mhandler_confg_file = system.file("extdata", 
  "config/mhandler.toml", package = "ngstk"), handler_funs = NULL, mhandler_funs = NULL, 
  handler_extra_params = NULL, mhandler_extra_params = NULL, outfn = NULL) {
  this_section <- "fusions_filter"
  meta_flag <- "meta"
  format_flag <- "format"
  params <- initial_params(config_file, config_list, input_type, this_section, 
    meta_flag, format_flag, handler_funs, mhandler_funs, handler_confg_file, 
    mhandler_confg_file)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  handler_funs <- params$handler_funs
  mhandler_funs <- params$mhandler_funs
  handler_data <- NULL
  for (i in 1:length(defined_cols)) {
    handler_data <- fusions_filter_handler(handler_data, config_input, defined_cols, 
      input_data, i, handler_funs, handler_extra_params)
  }
  handler_data <- fusions_filter_mhandler(handler_data, config_input, mhandler_funs, 
    mhandler_extra_params)
  if (!is.null(outfn)) {
    write.table(handler_data, outfn, sep = "\t", row.names = F, quote = F, col.names = T)
  }
  return(handler_data)
}

# main interface for fusions_filter data process
fusions_filter_handler <- function(handler_data, config_input, defined_cols, input_data, 
  index, handler_funs = NULL, extra_params) {
  handler_data <- handler(handler_data, config_input, defined_cols, input_data, 
    index, handler_funs, extra_params)
  return(handler_data)
}

fusions_filter_mhandler <- function(handler_data, config_input, mhandler_funs = NULL, 
  extra_params) {
  handler_data <- mhandler(handler_data, config_input, mhandler_funs, extra_params)
  return(handler_data)
}

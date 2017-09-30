#' Fusions hander_data filter that can be used to prepare the input data for downstream analysis
#' 
#' @param input_data A data frame containing the fusions cols (gene5, gene3, fusion_type)
#' @param input_type Fusion filter type
#' @param config_file ngstk filter configuration file path, default is 
#' system.file('extdata', 'config/filter.toml', package = 'ngstk')
#' @param hander_funs hander function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhander_funs hander function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param hander_extra_params Extra parameters pass to handler
#' @param mhander_extra_params Extra parameters pass to mhandler
#' system.file('extdata', 'config/filter.toml', package = 'ngstk')
#' @export
#' @return 
#' A data frame
#' @examples 
#' demo_file <- system.file('extdata', 
#' 'demo/proteinpaint/fusion2pp_fusioncatcher.txt', package = 'ngstk')
#' input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#' result <- fusions_filter(input_data)
fusions_filter <- function(input_data, input_type = "common", config_file = system.file("extdata", "config/filter.toml", 
  package = "ngstk"), hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, mhander_extra_params = NULL) {
  config_meta <- eval.config(value = "meta", config = "fusions_filter", file = config_file)
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  if (is.null(hander_funs)) {
    hander_funs <- config_meta[["defined_cols"]][["hander_funs"]]
  }
  if (is.null(mhander_funs)) {
    mhander_funs <- config_meta[["defined_cols"]][["mhander_funs"]]
  }
  config_format <- eval.config(value = "format", config = "fusions_filter", file = config_file)
  config_input <- config_format[[input_type]]
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- fusions_filter_handler(hander_data, config_input, defined_cols, input_data, i, hander_funs, 
      hander_extra_params)
  }
  hander_data <- fusions_filter_mhandler(hander_data, config_input, mhander_funs, mhander_extra_params)
  return(hander_data)
}

# main interface for fusions_filter data process
fusions_filter_handler <- function(hander_data, config_input, defined_cols, input_data, index, hander_funs = NULL, 
  extra_params) {
  hander_data <- handler(hander_data, config_input, defined_cols, input_data, index, hander_funs, extra_params)
  return(hander_data)
}

fusions_filter_mhandler <- function(hander_data, config_input, mhander_funs = NULL, extra_params) {
  hander_data <- mhandler(hander_data, config_input, mhander_funs, extra_params)
  return(hander_data)
}

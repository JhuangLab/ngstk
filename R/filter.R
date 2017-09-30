#' Fusions hander_data filter that can be used to prepare the input data for downstream analysis
#' 
#' @param input_data A data frame containing the fusions cols (gene5, gene3, fusion_type)
#' @param input_type Fusion filter type
#' @param config_file ngstk filter configuration file path, default is 
#' system.file('extdata', 'config/filter.toml', package = 'ngstk')
#' @param config_list ngstk filter configuration, default is NULL and 
#' read from config_file
#' @param hander_confg_file ngstk hander configuration file path, default is 
#' system.file('extdata', 'config/hander.toml', package = 'ngstk')
#' @param mhander_confg_file ngstk hander configuration file path, default is 
#' system.file('extdata', 'config/mhander.toml', package = 'ngstk')
#' @param hander_funs hander function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhander_funs hander function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param hander_extra_params Extra parameters pass to handler
#' @param mhander_extra_params Extra parameters pass to mhandler
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
fusions_filter <- function(input_data, input_type = "common", config_file = system.file("extdata", "config/filter.toml", 
  package = "ngstk"), config_list = NULL, hander_confg_file = system.file("extdata", "config/hander.toml", 
  package = "ngstk"), mhander_confg_file = system.file("extdata", "config/mhander.toml", package = "ngstk"), 
  hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, mhander_extra_params = NULL, outfn = NULL) {
  this_section <- "fusions_filter"
  meta_flag <- "meta"
  format_flag <- "format"
  params <- initial_params(config_file, config_list, input_type, this_section, meta_flag, format_flag, 
    hander_funs, mhander_funs, hander_confg_file, mhander_confg_file)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  hander_funs <- params$hander_funs
  mhander_funs <- params$mhander_funs
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- fusions_filter_handler(hander_data, config_input, defined_cols, input_data, i, hander_funs, 
      hander_extra_params)
  }
  hander_data <- fusions_filter_mhandler(hander_data, config_input, mhander_funs, mhander_extra_params)
  if (!is.null(outfn)) {
    write.table(hander_data, outfn, sep = "\t", row.names = F, quote = F, col.names = T)
  }
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

#' Function to convert mutation data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
#' read from config_file
#' @param hander_funs hander function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhander_funs hander function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param hander_extra_params Extra parameters pass to handler
#' @param mhander_extra_params Extra parameters pass to mhandler
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file('extdata', 
#' 'demo/proteinpaint/muts2pp_iseq.txt', package = 'ngstk')
#' input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#' disease <- 'T-ALL'
#' input_data <- data.frame(input_data, disease)
#' input_data$disease <- as.character(input_data$disease)
#' muts2pp(input_data, input_type = 'iseq')

muts2pp <- function(input_data, input_type = "iseq", config_file = system.file("extdata", "config/proteinpaint.toml", 
  package = "ngstk"), config_list = NULL, hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, 
  mhander_extra_params = NULL) {
  this_section <- "muts2pp"
  meta_flag <- "meta"
  format_flag <- "format"
  params <- initial_params(config_file, config_list, input_type, this_section, meta_flag, format_flag, 
    hander_funs, mhander_funs)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  hander_funs <- params$hander_funs
  mhander_funs <- params$mhander_funs
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- proteinpaint_handler(hander_data, config_input, defined_cols, input_data, i, hander_funs, 
      hander_extra_params)
  }
  hander_data <- proteinpaint_mhandler(hander_data, config_input, mhander_funs, mhander_extra_params)
  return(hander_data)
}

#' Function to convert fusion data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (fusioncatcher or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
#' read from config_file
#' @param hander_funs hander function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhander_funs hander function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param hander_extra_params Extra parameters pass to handler
#' @param mhander_extra_params Extra parameters pass to mhandler
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file('extdata', 
#' 'demo/proteinpaint/fusions2pp_fusioncatcher.txt', package = 'ngstk')
#' input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#' disease <- 'B-ALL'
#' sampletype <- 'diagnose'
#' input_data <- data.frame(input_data, disease, sampletype)
#' input_data$disease <- as.character(input_data$disease)
#' hander_data <- fusions2pp(input_data, input_type = 'fusioncatcher')
fusions2pp <- function(input_data, input_type = "fusioncatcher", config_file = system.file("extdata", "config/proteinpaint.toml", 
  package = "ngstk"), config_list = NULL, hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, 
  mhander_extra_params = NULL) {
  this_section <- "fusions2pp"
  meta_flag <- "meta"
  format_flag <- "format"
  params <- initial_params(config_file, config_list, input_type, this_section, meta_flag, format_flag, 
    hander_funs, mhander_funs)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  hander_funs <- params$hander_funs
  mhander_funs <- params$mhander_funs
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- proteinpaint_handler(hander_data, config_input, defined_cols, input_data, i, hander_funs, 
      hander_extra_params)
  }
  hander_data <- proteinpaint_mhandler(hander_data, config_input, mhander_funs, mhander_extra_params)
  return(hander_data)
}

#' Function to convert fusion data to ProteinPaint heatmap meta rows \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (fusioncatcher or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
#' read from config_file
#' @param hander_funs hander function for single colnum, 
#' default is NULL and get value from config_file
#' @param mhander_funs hander function for mulitple colnums,
#' #' default is NULL and get value from config_file
#' @param hander_extra_params Extra parameters pass to handler
#' @param mhander_extra_params Extra parameters pass to mhandler
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file('extdata', 
#' 'demo/proteinpaint/fusions2pp_fusioncatcher.txt', package = 'ngstk')
#' input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
#' disease <- 'B-ALL'
#' sampletype <- 'diagnose'
#' input_data <- data.frame(input_data, disease, sampletype)
#' input_data$disease <- as.character(input_data$disease)
#' hander_data <- fusions2pp_meta(input_data, input_type = 'fusioncatcher')
fusions2pp_meta <- function(input_data, input_type = "fusioncatcher", config_file = system.file("extdata", 
  "config/proteinpaint.toml", package = "ngstk"), config_list = NULL, hander_funs = NULL, mhander_funs = NULL, 
  hander_extra_params = NULL, mhander_extra_params = NULL) {
  this_section <- "fusions2pp_meta"
  meta_flag <- "meta"
  format_flag <- "format"
  params <- initial_params(config_file, config_list, input_type, this_section, meta_flag, format_flag, 
    hander_funs, mhander_funs)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  hander_funs <- params$hander_funs
  mhander_funs <- params$mhander_funs
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- proteinpaint_handler(hander_data, config_input, defined_cols, input_data, i, hander_funs, 
      hander_extra_params)
  }
  hander_data <- proteinpaint_mhandler(hander_data, config_input, mhander_funs, mhander_extra_params)
  return(hander_data)
}


# main interface for proteinpaint data process
proteinpaint_handler <- function(hander_data, config_input, defined_cols, input_data, index, hander_funs = NULL, 
  extra_params = NULL) {
  
  hander_data <- handler(hander_data, config_input, defined_cols, input_data, index, hander_funs = hander_funs, 
    extra_params = extra_params)
  return(hander_data)
}

proteinpaint_mhandler <- function(hander_data, config_input, mhander_funs = NULL, extra_params = NULL) {
  
  hander_data <- mhandler(hander_data, config_input, mhander_funs, extra_params)
  return(hander_data)
}

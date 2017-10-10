#' Function to convert mutation data to MutationMapper
#' \url{http://www.cbioportal.org/mutation_mapper.jsp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
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
#' @param outfn Default is NULL and not output the result to file
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
#' muts2mutation_mapper(input_data, input_type = 'iseq')

muts2mutation_mapper <- function(input_data, input_type = "iseq", config_file = system.file("extdata", "config/cbioportal.toml", 
  package = "ngstk"), config_list = NULL, hander_confg_file = system.file("extdata", "config/hander.toml", 
  package = "ngstk"), mhander_confg_file = system.file("extdata", "config/mhander.toml", package = "ngstk"), 
  hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, mhander_extra_params = NULL, outfn = NULL) {
  data_format_converter(input_data, input_type, config_file, config_list, hander_confg_file, mhander_confg_file, 
    hander_funs, mhander_funs, hander_extra_params, mhander_extra_params, outfn, "muts2mutation_mapper", 
    "default_hander_api", "default_mhandler_api")
}

#' Function to convert mutation data to Oncoprinter
#' \url{http://www.cbioportal.org/oncoprinter.jsp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
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
#' @param outfn Default is NULL and not output the result to file
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
#' muts2oncoprinter(input_data, input_type = 'iseq')
muts2oncoprinter <- function(input_data, input_type = "iseq", config_file = system.file("extdata", "config/cbioportal.toml", 
  package = "ngstk"), config_list = NULL, hander_confg_file = system.file("extdata", "config/hander.toml", 
  package = "ngstk"), mhander_confg_file = system.file("extdata", "config/mhander.toml", package = "ngstk"), 
  hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, mhander_extra_params = NULL, outfn = NULL) {
  data_format_converter(input_data, input_type, config_file, config_list, hander_confg_file, mhander_confg_file, 
    hander_funs, mhander_funs, hander_extra_params, mhander_extra_params, outfn, "muts2oncoprinter", 
    "default_hander_api", "default_mhandler_api")
}

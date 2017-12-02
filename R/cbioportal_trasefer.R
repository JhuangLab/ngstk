#' Function to convert mutation data to MutationMapper
#' \url{http://www.cbioportal.org/mutation_mapper.jsp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file('extdata', 'config/proteinpaint.toml', package = 'ngstk')
#' @param config_list ngstk ProteinPaint configuration, default is NULL and 
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

muts2mutation_mapper <- function(input_data, input_type = "iseq", config_file = system.file("extdata", 
  "config/cbioportal.toml", package = "ngstk"), config_list = NULL, handler_confg_file = system.file("extdata", 
  "config/handler.toml", package = "ngstk"), mhandler_confg_file = system.file("extdata", 
  "config/mhandler.toml", package = "ngstk"), handler_funs = NULL, mhandler_funs = NULL, 
  handler_extra_params = NULL, mhandler_extra_params = NULL, outfn = NULL) {
  data_format_converter(input_data, input_type, config_file, config_list, handler_confg_file, 
    mhandler_confg_file, handler_funs, mhandler_funs, handler_extra_params, mhandler_extra_params, 
    outfn, "muts2mutation_mapper", "default_handler_api", "default_mhandler_api")
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
muts2oncoprinter <- function(input_data, input_type = "iseq", config_file = system.file("extdata", 
  "config/cbioportal.toml", package = "ngstk"), config_list = NULL, handler_confg_file = system.file("extdata", 
  "config/handler.toml", package = "ngstk"), mhandler_confg_file = system.file("extdata", 
  "config/mhandler.toml", package = "ngstk"), handler_funs = NULL, mhandler_funs = NULL, 
  handler_extra_params = NULL, mhandler_extra_params = NULL, outfn = NULL) {
  data_format_converter(input_data, input_type, config_file, config_list, handler_confg_file, 
    mhandler_confg_file, handler_funs, mhandler_funs, handler_extra_params, mhandler_extra_params, 
    outfn, "muts2oncoprinter", "default_handler_api", "default_mhandler_api")
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
#' fusions2oncoprinter(input_data, input_type = 'fusioncatcher')
fusions2oncoprinter <- function(input_data, input_type = "fusioncatcher", config_file = system.file("extdata", 
  "config/cbioportal.toml", package = "ngstk"), config_list = NULL, handler_confg_file = system.file("extdata", 
  "config/handler.toml", package = "ngstk"), mhandler_confg_file = system.file("extdata", 
  "config/mhandler.toml", package = "ngstk"), handler_funs = NULL, mhandler_funs = NULL, 
  handler_extra_params = NULL, mhandler_extra_params = NULL, outfn = NULL) {
  data_format_converter(input_data, input_type, config_file, config_list, handler_confg_file, 
    mhandler_confg_file, handler_funs, mhandler_funs, handler_extra_params, mhandler_extra_params, 
    outfn, "fusions2oncoprinter", "default_handler_api", "default_mhandler_api")
}

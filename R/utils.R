#' Function to generate tools path object
#' @param config.file Path of tools configuration file (json, ini, yaml and toml be supported)
#' @param config.list List object of tools that all of tools path (exclude those without names). 
#' @param config.vec Vector object of tools that all of tools path (exclude those without names). 
#' @param eval.params Params pass to configr::eval.config
#' 
#' @return
#' List object contain the tools path that can be used by other function in ngstk package
#' @export
#' @examples
#' config.file <- system.file('extdata', 'demo/tools_config.json', package = 'ngstk')
#' config.list <- list(gatk = '/path/gatk')
#' config.vec <- c('/path/samtools')
#' names(config.vec) <- 'samtools'
#' tools <- set_tools(config.file, config.list, config.vec, 
#'                   eval.params = list(config = 'tools'))

set_tools <- function(config.file = "", config.list = list(), config.vec = c(), eval.params = list()) {
  
  config.list.1 <- NULL
  config.list.2 <- NULL
  config.list.3 <- NULL
  tools <- list()
  
  if (config.file != "") {
    params <- configr::config.list.merge(eval.params, list(file = config.file))
    config <- do.call(configr::eval.config, params)
    config.list.1 <- config[names(config) != ""]
    tools <- configr::config.list.merge(tools, config.list.1)
  }
  if (is.list(config.list) && length(config.list) > 0) {
    config.list.2 <- config.list[names(config.list) != ""]
    tools <- configr::config.list.merge(tools, config.list.2)
  }
  if (is.vector(config.vec) && length(config.vec) > 0) {
    config.vec <- config.vec[names(config.vec) != ""]
    config.list.3 <- as.list(config.vec)
    tools <- configr::config.list.merge(tools, config.list.3)
  }
  return(tools)
}


#' Function to get a series defined theme colors
#' 
#' @param theme Colors theme, e.g. default, red_blue
#' @param theme_config_file Theme configuration file, default is 
#' system.file('extdata', 'config/theme.toml', package = 'ngstk')
#' @export
#' @return
#' A character
#' @examples 
#' red_blue <- set_colors('red_blue')
#' default <- set_colors('default')
set_colors <- function(theme = NULL, theme_config_file = NULL) {
  if (is.null(theme_config_file)) {
    theme_config_file <- system.file("extdata", "config/theme.toml", package = "ngstk")
  }
  if (is.null(theme)) {
    theme <- "default"
  }
  colors <- eval.config(value = "colors", config = theme, file = theme_config_file)
  return(colors)
}

# Get config value (2 depth)
get_config_value <- function(config_input, level_1, level_2) {
  config_input[[level_1]][[level_2]]
}


# initial config_meta_format

initial_params <- function(config_file, config_list, input_type, this_section, meta_flag, format_flag, hander_funs = NULL, 
  mhander_funs = NULL, hander_confg_file = NULL, mhander_confg_file = NULL) {
  if (is.null(config_list)) {
    config_meta <- eval.config(value = meta_flag, config = this_section, file = config_file)
    config_format <- eval.config(value = format_flag, config = this_section, file = config_file)
  } else {
    config_meta <- config_list[[this_section]][[meta_flag]]
    config_format <- config_list[[this_section]][[format_flag]]
  }
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  if (is.null(hander_funs)) {
    hander_lib <- config_meta[["defined_cols"]][["hander_lib"]]
    if (is.null(hander_lib)) {
      hander_lib <- "default_handers"
    }
    hander_lib_data <- eval.config(value = hander_lib, config = "hander", file = hander_confg_file)
    hander_funs <- hander_lib_data$hander_funs
  }
  if (is.null(mhander_funs)) {
    mhander_lib <- config_meta[["defined_cols"]][["mhander_lib"]]
    if (is.null(mhander_lib)) {
      mhander_lib <- "default_mhanders"
    }
    mhander_lib_data <- eval.config(value = mhander_lib, config = "mhander", file = mhander_confg_file)
    mhander_funs <- mhander_lib_data$mhander_funs
  }
  config_input <- config_format[[input_type]]
  return(list(config_meta = config_meta, config_format = config_format, config_input = config_input, defined_cols = defined_cols, 
    hander_funs = hander_funs, mhander_funs = mhander_funs))
}

# format converter
data_format_converter <- function(input_data, input_type = "", config_file = "", config_list = NULL, hander_confg_file = "", 
  mhander_confg_file = "", hander_funs = NULL, mhander_funs = NULL, hander_extra_params = NULL, mhander_extra_params = NULL, 
  outfn = NULL, function_name = "", hander_api = "", mhander_api = "", meta_flag = "meta", format_flag = "format") {
  
  params <- initial_params(config_file, config_list, input_type, function_name, meta_flag, format_flag, 
    hander_funs, mhander_funs, hander_confg_file, mhander_confg_file)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  hander_funs <- params$hander_funs
  mhander_funs <- params$mhander_funs
  hander_data <- NULL
  for (i in 1:length(defined_cols)) {
    hander_data <- do.call(hander_api, list(hander_data = hander_data, config_input = config_input, 
      defined_cols = defined_cols, input_data = input_data, index = i, hander_funs = hander_funs, 
      extra_params = hander_extra_params))
  }
  hander_data <- do.call(mhander_api, list(hander_data = hander_data, config_input = config_input, mhander_funs = mhander_funs, 
    extra_params = hander_extra_params))
  if (!is.null(outfn)) {
    write.table(hander_data, outfn, sep = "\t", row.names = F, quote = F, col.names = T)
  }
  return(hander_data)
}

default_hander_api <- function(hander_data, config_input, defined_cols, input_data, index, hander_funs = NULL, 
  extra_params = NULL) {
  
  hander_data <- handler(hander_data, config_input, defined_cols, input_data, index, hander_funs = hander_funs, 
    extra_params = extra_params)
  return(hander_data)
}

default_mhandler_api <- function(hander_data, config_input, mhander_funs = NULL, extra_params = NULL) {
  
  hander_data <- mhandler(hander_data, config_input, mhander_funs, extra_params)
  return(hander_data)
}

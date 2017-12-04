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

#' Process the input file a batch of one batch
#' @param filename Filename need to process
#' @param batch_lines Batch lines to process the data, default 10000000
#' @param handler The function to process the data
#' @param param_names Hander function required parameter names
#' @param extra_fread_params Extra fread parameters in read data step,
#' default is list(sep = '\\n', header = TRUE, return_1L = TRUE), return_1L to get x[[1L]]
#' @param extra_params Extra paramemters pass to handler function
#' @param start_index default is 1, control the skip rows, n = (i-1) * batch_lines
#' @export
#' @examples
#' dat <- data.frame(a=1:100, b=1:100)
#' filename <- tempfile()
#' write.table(dat, filename, sep = '\t', row.names = FALSE, quote = FALSE)
#' handler_fun <- function(x, i = 1) {
#'   return(x[i])
#' }
#' batch_file(filename, 10, handler_fun)
batch_file <- function(filename = "", batch_lines = 1e+07, handler = NULL, param_names = c("x", 
  "i"), extra_fread_params = list(sep = "\n", header = FALSE, return_1L = TRUE), 
  extra_params = list(), start_index = 1) {
  old_op <- options()
  options(scipen = 200)
  i <- start_index
  pool <- "x"
  if (start_index != 1) {
    status <- lapply(1:start_index, function(x) {
      return(NA)
    })
    names(status)[1:(start_index - 1)] <- 1:(start_index - 1)
  } else {
    status <- NULL
  }
  return_1L <- extra_fread_params$return_1L
  extra_fread_params$return_1L <- NULL
  while (TRUE) {
    skip <- as.numeric((i - 1) * batch_lines)
    if (i != 1) {
      extra_fread_params$header = FALSE
    }
    fread_params <- config.list.merge(list(input = filename, nrows = batch_lines, 
      skip = skip), extra_fread_params)
    if (return_1L) {
      assign(pool[1], value = do.call(fread, fread_params)[[1L]])
    } else {
      assign(pool[1], value = do.call(fread, fread_params))
    }
    x <- get(pool[1])
    params <- list(x = x, i = i)
    names(params) <- param_names
    params <- config.list.merge(params, extra_params)
    status.tmp <- do.call(handler, params)
    if (is.null(status)) {
      status <- list(i = status.tmp)
      names(status) <- i
    } else {
      status <- config.list.merge(status, list(i = status.tmp))
      names(status)[i] <- i
    }
    if (return_1L && length(get(pool[1])) < batch_lines) {
      break
    } else if (!return_1L && nrow(x) < batch_lines) {
      break
    } else {
      i <- i + 1
    }
  }
  options(old_op)
  status[length(status)] <- NULL
  return(status)
}

# Get config value (2 depth)
get_config_value <- function(config_input, level_1, level_2) {
  config_input[[level_1]][[level_2]]
}


# initial config_meta_format

initial_params <- function(config_file, config_list, input_type, this_section, meta_flag, 
  format_flag, handler_funs = NULL, mhandler_funs = NULL, handler_confg_file = NULL, 
  mhandler_confg_file = NULL) {
  if (is.null(config_list)) {
    config_meta <- eval.config(value = meta_flag, config = this_section, file = config_file)
    config_format <- eval.config(value = format_flag, config = this_section, 
      file = config_file)
  } else {
    config_meta <- config_list[[this_section]][[meta_flag]]
    config_format <- config_list[[this_section]][[format_flag]]
  }
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  if (is.null(handler_funs)) {
    handler_lib <- config_meta[["defined_cols"]][["handler_lib"]]
    if (is.null(handler_lib)) {
      handler_lib <- "default_handlers"
    }
    handler_lib_data <- eval.config(value = handler_lib, config = "handler", 
      file = handler_confg_file)
    handler_funs <- handler_lib_data$handler_funs
  }
  if (is.null(mhandler_funs)) {
    mhandler_lib <- config_meta[["defined_cols"]][["mhandler_lib"]]
    if (is.null(mhandler_lib)) {
      mhandler_lib <- "default_mhandlers"
    }
    mhandler_lib_data <- eval.config(value = mhandler_lib, config = "mhandler", 
      file = mhandler_confg_file)
    mhandler_funs <- mhandler_lib_data$mhandler_funs
  }
  config_input <- config_format[[input_type]]
  return(list(config_meta = config_meta, config_format = config_format, config_input = config_input, 
    defined_cols = defined_cols, handler_funs = handler_funs, mhandler_funs = mhandler_funs))
}

# format converter
data_format_converter <- function(input_data, input_type = "", config_file = "", 
  config_list = NULL, handler_confg_file = "", mhandler_confg_file = "", handler_funs = NULL, 
  mhandler_funs = NULL, handler_extra_params = NULL, mhandler_extra_params = NULL, 
  outfn = NULL, function_name = "", handler_api = "", mhandler_api = "", meta_flag = "meta", 
  format_flag = "format") {
  
  params <- initial_params(config_file, config_list, input_type, function_name, 
    meta_flag, format_flag, handler_funs, mhandler_funs, handler_confg_file, 
    mhandler_confg_file)
  config_input <- params$config_input
  defined_cols <- params$defined_cols
  config_input <- params$config_input
  handler_funs <- params$handler_funs
  mhandler_funs <- params$mhandler_funs
  handler_data <- NULL
  for (i in 1:length(defined_cols)) {
    handler_data <- do.call(handler_api, list(handler_data = handler_data, config_input = config_input, 
      defined_cols = defined_cols, input_data = input_data, index = i, handler_funs = handler_funs, 
      extra_params = handler_extra_params))
  }
  handler_data <- do.call(mhandler_api, list(handler_data = handler_data, config_input = config_input, 
    mhandler_funs = mhandler_funs, extra_params = handler_extra_params))
  if (!is.null(outfn)) {
    write.table(handler_data, outfn, sep = "\t", row.names = F, quote = F, col.names = T)
  }
  return(handler_data)
}

default_handler_api <- function(handler_data, config_input, defined_cols, input_data, 
  index, handler_funs = NULL, extra_params = NULL) {
  
  handler_data <- handler(handler_data, config_input, defined_cols, input_data, 
    index, handler_funs = handler_funs, extra_params = extra_params)
  return(handler_data)
}

default_mhandler_api <- function(handler_data, config_input, mhandler_funs = NULL, 
  extra_params = NULL) {
  
  handler_data <- mhandler(handler_data, config_input, mhandler_funs, extra_params)
  return(handler_data)
}

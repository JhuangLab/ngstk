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

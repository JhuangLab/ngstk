#' Function to convert mutation data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file("extdata", "config/proteinpaint.toml", package = "ngstk")
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file("extdata", "demo/mut2pp_iseq.txt", package = "ngstk")
#' input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
#' disease <- "T-ALL"
#' input_data <- data.frame(input_data, disease)
#' input_data$disease <- as.character(input_data$disease)
#' mut2pp(input_data, input_type = "iseq")

mut2pp <- function(input_data, input_type = "iseq", 
                   config_file = system.file("extdata", "config/proteinpaint.toml", package = "ngstk")) {
  config_meta <- eval.config(value = "meta", config = "mut2pp", file = config_file)
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  config_format <- eval.config(value = "format", config = "mut2pp", file = config_file)
  config_input <- config_format[[input_type]]
  result <- NULL
  for(i in 1:length(defined_cols)) {
    result <- handler_proteinpaint(result, config_input, defined_cols, 
                                   input_data, i)
  }
  return(result)
}

#' Function to convert fusion data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (fusioncatcher or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' system.file("extdata", "config/proteinpaint.toml", package = "ngstk")
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file("extdata", "demo/fusion2pp_fusioncatcher.txt", package = "ngstk")
#' input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
#' disease <- "B-ALL"
#' sampletype <- "diagnose"
#' input_data <- data.frame(input_data, disease, sampletype)
#' input_data$disease <- as.character(input_data$disease)
#' result <- fusion2pp(input_data, input_type = "fusioncatcher")
fusion2pp <- function(input_data, input_type = "fusioncatcher", 
                      config_file = system.file("extdata", "config/proteinpaint.toml", 
                                                package = "ngstk")) {
  config_meta <- eval.config(value = "meta", config = "fusion2pp", file = config_file)
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  config_format <- eval.config(value = "format", config = "fusion2pp", file = config_file)
  config_input <- config_format[[input_type]]
  result <- NULL
  for(i in 1:length(defined_cols)) {
    result <- handler_proteinpaint(result, config_input, defined_cols, 
                                   input_data, i)
  }
  return(result)
}

#' Function to convert fusion data to ProteinPaint heatmap meta rows \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (fusioncatcher or others)
#' @param config_file ngstk ProteinPaint configuration file path, default is 
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' demo_file <- system.file("extdata", "demo/fusion2pp_fusioncatcher.txt", package = "ngstk")
#' input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
#' disease <- "B-ALL"
#' sampletype <- "diagnose"
#' input_data <- data.frame(input_data, disease, sampletype)
#' input_data$disease <- as.character(input_data$disease)
#' result <- fusion2pp_meta(input_data, input_type = "fusioncatcher")
fusion2pp_meta <- function(input_data, input_type = "fusioncatcher", 
                           config_file = system.file("extdata", "config/proteinpaint.toml", 
                                                     package = "ngstk")) {
  config_meta <- eval.config(value = "meta", config = "fusion2pp_meta", file = config_file)
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  config_format <- eval.config(value = "format", config = "fusion2pp_meta", file = config_file)
  config_input <- config_format[[input_type]]
  result <- NULL
  for(i in 1:length(defined_cols)) {
    result <- handler_proteinpaint(result, config_input, defined_cols, 
                                   input_data, i)
  }
  return(result)
}

### Framework utils function ###

get_config_value <- function(config_input, level_1, level_2) {
  config_input[[level_1]][[level_2]]
}

# na.replace
handler_na_replace <- function(result, config_input, col_use, last.col) {
  na.replace <- get_config_value(config_input, col_use, "na_replace")
  if (!is.null(na.replace)) {
    index <- is.na(result[,last.col])
    if (any(index)) {
      result[index,last.col] <- na.replace
    }
  }
  return(result)
}

# map old value to new value
handler_value_map <- function(result, config_input, col_use, last.col) {
  raw_value <- get_config_value(config_input, col_use, "raw")
  new_value <- get_config_value(config_input, col_use, "new")
  if (!is.null(raw_value)) {
    for(x in 1:length(raw_value)) {
      result[,last.col] <- str_replace(result[,last.col], raw_value[x], new_value[x])
    }
  }
  return(result)
}

# use str_extract get value
handler_extract <- function(result, config_input, col_use, last.col) {
  # Process extract
  extract_pattern <- get_config_value(config_input, col_use, "extract_pattern")
  if (!is.null(extract_pattern)) {
    result[,last.col] <- str_extract(result[,last.col], extract_pattern)
  }
  return(result)
}


# use str_split get value
handler_split <- function(result, config_input, col_use, last.col) {
  split_flag <- get_config_value(config_input, col_use, "split_flag")
  split_index <- get_config_value(config_input, col_use, "split_index")
  split_index <- as.numeric(split_index)
  if (!is.null(split_flag)) {
    result[,last.col] <- sapply(str_split(result[,last.col], split_flag), 
                                function(x) return(x[split_index]))
  }
  return(result)
}

# add prefix
handler_prefix <- function(result, config_input, col_use, last.col) {
  prefix_flag <- get_config_value(config_input, col_use, "prefix_flag")
  if (!is.null(prefix_flag)) {
    result[,last.col] <- unname(sapply(result[,last.col], 
                                function(x) {
                                  if (!str_detect(x, paste0("^", prefix_flag))){
                                    return(paste0(prefix_flag, x))
                                  } else {
                                    return(x)
                                  }
                                }))
  }
  return(result)
}

# add postfix
handler_postfix <- function(result, config_input, col_use, last.col) {
  postfix_flag <- get_config_value(config_input, col_use, "postfix_flag")
  if (!is.null(postfix_flag)) {
    result[,last.col] <- unname(sapply(result[,last.col], 
                                       function(x) {
                                         if (!str_detect(x, paste0(postfix_flag, "$"))){
                                           return(paste0(postfix_flag, x))
                                         } else {
                                           return(x)
                                         }
                                       }))
  }
  return(result)
}

# lower
handler_lower <- function(result, config_input, col_use, last.col) {
  flag <- get_config_value(config_input, col_use, "lower")
  if (!is.null(flag)) {
    result[,last.col] <- tolower(result[,last.col])
  }
  return(result)
}

# upper
handler_upper <- function(result, config_input, col_use, last.col) {
  flag <- get_config_value(config_input, col_use, "upper")
  if (!is.null(flag)) {
    result[,last.col] <- tolower(result[,last.col])
  }
  return(result)
}

# main interface for proteinpaint data process
handler_proteinpaint <- function(result, config_input, defined_cols, input_data, last.col) {
  col_use <- defined_cols[last.col]
  valid_cols <- get_config_value(config_input, col_use, "alias")
  valid_cols <- tolower(valid_cols)
  input_data_col_lower <- tolower(colnames(input_data))
  col_use_index <- which(input_data_col_lower %in% valid_cols)[1]
  old_op <- options()
  options(stringsAsFactors = F)
  if (is.na(col_use_index)) {
    empty.col <- rep("NA", nrow(result)) 
    result <- cbind(result, empty.col)
    warning(sprintf("%s were not exists or not be recognize correctly in input data!", col_use))
  } else {
    result <- cbind(result, input_data[[col_use_index]])
  }
  result <- as.data.frame(result)
  colnames(result)[last.col] <- col_use
  result <- handler_na_replace(result, config_input, col_use, last.col)
  result <- handler_lower(result, config_input, col_use, last.col)
  result <- handler_upper(result, config_input, col_use, last.col)
  result <- handler_value_map(result, config_input, col_use, last.col)
  result <- handler_extract(result, config_input, col_use, last.col)
  result <- handler_split(result, config_input, col_use, last.col)
  result <- handler_prefix(result, config_input, col_use, last.col)
  result <- handler_postfix(result, config_input, col_use, last.col)
  options(old_op)
  return(result)
}

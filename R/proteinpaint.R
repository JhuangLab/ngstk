#' Function to convert mutation data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param input_type Point the input data format (iseq or others)
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

mut2pp <- function(input_data, input_type = "iseq") {
  config_file <- system.file("extdata", "config/proteinpaint.toml", package = "ngstk")
  config_meta <- eval.config(value = "meta", config = "mut2pp", file = config_file)
  defined_cols <- config_meta[["defined_cols"]][["colnames"]]
  config_format <- eval.config(value = "format", config = "mut2pp", file = config_file)
  config_input <- config_format[[input_type]]
  result <- NULL
  for(i in 1:length(defined_cols)) {
    col_use <- defined_cols[i]
    valid_cols <- get_config_value(config_input, col_use, "alias")
    input_data_col_lower <- tolower(colnames(input_data))
    col_use_index <- which(input_data_col_lower %in% valid_cols)[1]
    result <- cbind(result, input_data[[col_use_index]])
    colnames(result)[i] <- col_use
    # Process Na value
    na.replace <- get_config_value(config_input, col_use, "na_replace")
    if (!is.null(na.replace)) {
      index <- is.na(result[,i])
      result[index,i] <- na.replace
    }
    # Process value map
    raw_value <- get_config_value(config_input, col_use, "raw")
    new_value <- get_config_value(config_input, col_use, "new")
    if (!is.null(raw_value)) {
      for(x in 1:length(raw_value)) {
        result[,i] <- str_replace(result[,i], raw_value[x], new_value[x])
      }
    }
    # Process extract
    extract_pattern <- get_config_value(config_input, col_use, "extract_pattern")
    if (!is.null(extract_pattern)) {
      result[,i] <- str_extract(result[,i], extract_pattern)
    }
  }
  return(result)
}

get_config_value <- function(config_input, level_1, level_2) {
  config_input[[level_1]][[level_2]]
}



#' Function to convert fusion data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param format Point the input data format (annovar or others)
#' @param order Input data cols order (chr, start, end, trascript and aachange)
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' example <- NULL
fusion2pp <- function(input_data, format = "fusioncatcher", 
                                    order = list(gene5 = 1, gene3 = 2)
) {
  if (format == "fusioncatcher") {
    
  }
}

#' Function to convert fusion data to ProteinPaint heatmap meta rows \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input (heatmap meta rows).
#' @param format Point the input data format (annovar or others)
#' @param order Input data cols order (chr, start, end, trascript and aachange)
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' example <- NULL
fusion2pp_meta <- function(input_data, format = "fusioncatcher", 
                      order = list(sample = 1, gene5 = 2, gene3 = 3)) {
  if (format == "fusioncatcher") {
    
  }
}
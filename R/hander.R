#' Function to show all avaliabe handler function
#' 
#' @param hander_lib hander lib name
#' @param show_all_funs Default is TRUE and to show all functions in the hander_lib
#' @param show_code Default is NULL, select hander you want to see its source code
#' @param show_description Default is FALSE
#' @param hander_confg_file ngstk hander configuration file path, default is
#' system.file('extdata', 'config/hander.toml', package = 'ngstk')
#' @return 
#' NULL
#' @export
#' @examples 
#' show_handers(show_description = TRUE)
#' show_handers(show_description = FALSE, show_all_funs = FALSE, 
#'             show_code = 'handler_na_replace')
show_handers <- function(hander_lib = "default_handers", show_all_funs = TRUE, show_code = NULL, show_description = FALSE, 
  hander_confg_file = system.file("extdata", "config/hander.toml", package = "ngstk")) {
  hander_lib_data <- eval.config(value = hander_lib, config = "hander", file = hander_confg_file)
  if (show_all_funs) {
    hander_funs <- paste0(hander_lib_data[["hander_funs"]], collapse = ", ")
    msg <- sprintf("%s were exists in file %s %s.", hander_funs, hander_confg_file, hander_lib)
    message(paste0("[INFO] ", msg))
  }
  if (show_description) {
    description <- hander_lib_data$description
    message(description)
  }
  if (!is.null(show_code)) {
    for (i in show_code) {
      print(eval(parse(text = i)))
    }
  }
}

#' Function to show all avaliabe mhandler function
#' 
#' @param mhander_lib hander lib name 
#' @param show_all_funs Default is TRUE and to show all functions in the hander_lib
#' @param show_code Default is NULL, select hander you want to see its source code
#' @param show_description Default is FALSE
#' @param mhander_confg_file ngstk hander configuration file path, default is 
#' system.file('extdata', 'config/hander.toml', package = 'ngstk')
#' @return 
#' NULL
#' @export
#' @examples 
#' show_mhanders(show_description = TRUE)
#' show_mhanders(show_description = FALSE, show_all_funs = FALSE, 
#'              show_code = 'mhander_fusions_anyfull_match')
show_mhanders <- function(mhander_lib = "default_mhanders", show_all_funs = TRUE, show_code = NULL, show_description = FALSE, 
  mhander_confg_file = system.file("extdata", "config/mhander.toml", package = "ngstk")) {
  mhander_lib_data <- eval.config(value = mhander_lib, config = "mhander", file = mhander_confg_file)
  if (show_all_funs) {
    mhander_funs <- paste0(mhander_lib_data[["mhander_funs"]], collapse = ", ")
    msg <- sprintf("%s were exists in file %s %s.", mhander_funs, mhander_confg_file, mhander_lib)
    message(paste0("[INFO] ", msg))
  }
  if (show_description) {
    description <- mhander_lib_data$description
    message(description)
  }
  if (!is.null(show_code)) {
    for (i in show_code) {
      print(eval(parse(text = i)))
    }
  }
}

# Function that can be uesd to process data
handler <- function(hander_data, config_input, defined_cols, input_data, index, hander_funs = NULL, extra_params = NULL) {
  defined_col <- defined_cols[index]
  valid_col_index <- get_valid_col_index(config_input, defined_cols[index], input_data)[1]
  old_op <- options()
  options(stringsAsFactors = F)
  if (is.na(valid_col_index)) {
    empty.col <- rep("NA", nrow(hander_data))
    hander_data <- cbind(hander_data, empty.col)
    warning(sprintf("%s were not exists or not be recognize correctly in input data!", defined_col))
  } else {
    hander_data <- cbind(hander_data, input_data[[valid_col_index]])
  }
  hander_data <- as.data.frame(hander_data)
  colnames(hander_data)[index] <- defined_col
  if (!is.null(hander_funs)) {
    for (i in hander_funs) {
      hander_data <- do.call(i, list(hander_data = hander_data, config_input = config_input, defined_col = defined_col, 
        index = index, extra_params = extra_params))
    }
    
  }
  options(old_op)
  return(hander_data)
}

# use config alias value to get the candidate colum index
get_valid_col_index <- function(config_input, defined_col, input_data) {
  valid_cols <- get_config_value(config_input, defined_col, "alias")
  if (is.null(valid_cols)) {
    valid_cols <- defined_col
  } else {
    valid_cols <- c(valid_cols, defined_col)
  }
  valid_cols <- tolower(valid_cols)
  valid_cols <- unique(valid_cols)
  input_data_col_lower <- tolower(colnames(input_data))
  valid_col_index <- which(input_data_col_lower %in% valid_cols)
  return(valid_col_index)
}

### Framework util functions ###

# na.replace
handler_na_replace <- function(hander_data, config_input, defined_col, index, extra_params = list(na_replace_flag = TRUE)) {
  flag <- extra_params$na_replace_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  na_replace <- get_config_value(config_input, defined_col, "na_replace")
  if (!is.null(na_replace)) {
    index <- is.na(hander_data[, index])
    if (any(index)) {
      hander_data[index, index] <- na_replace
    }
  }
  return(hander_data)
}

# map old value to new value
handler_value_map <- function(hander_data, config_input, defined_col, index, extra_params = list(value_map_flag = TRUE)) {
  flag <- extra_params$value_map_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  raw_value <- get_config_value(config_input, defined_col, "raw")
  new_value <- get_config_value(config_input, defined_col, "new")
  if (!is.null(raw_value)) {
    for (x in 1:length(raw_value)) {
      hander_data[, index] <- str_replace(hander_data[, index], raw_value[x], new_value[x])
    }
  }
  return(hander_data)
}

# use str_extract get value
handler_extract <- function(hander_data, config_input, defined_col, index, extra_params = list(extract_flag = TRUE)) {
  flag <- extra_params$extract_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  # Process extract
  extract_pattern <- get_config_value(config_input, defined_col, "extract_pattern")
  for (i in extract_pattern) {
    if (!is.null(i)) {
      hander_data[, index] <- str_extract(hander_data[, index], i)
    }
  }
  
  return(hander_data)
}


# use str_split get value
handler_split <- function(hander_data, config_input, defined_col, index, extra_params = list(split_flag = TRUE)) {
  flag <- extra_params$split_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  split_marker <- get_config_value(config_input, defined_col, "split_marker")
  split_index <- get_config_value(config_input, defined_col, "split_index")
  split_index <- as.numeric(split_index)
  if (!is.null(split_marker)) {
    hander_data[, index] <- sapply(str_split(hander_data[, index], split_marker), function(x) return(x[split_index]))
  }
  return(hander_data)
}

# add prefix
handler_prefix <- function(hander_data, config_input, defined_col, index, extra_params = list(prefix_flag = TRUE)) {
  flag <- extra_params$prefix_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  prefix_flag <- get_config_value(config_input, defined_col, "prefix_marker")
  if (!is.null(prefix_flag)) {
    hander_data[, index] <- unname(sapply(hander_data[, index], function(x) {
      if (!str_detect(x, paste0("^", prefix_flag))) {
        return(paste0(prefix_flag, x))
      } else {
        return(x)
      }
    }))
  }
  return(hander_data)
}

# add postfix
handler_postfix <- function(hander_data, config_input, defined_col, index, extra_params = list(postfix_flag = TRUE)) {
  flag <- extra_params$postfix_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  postfix_marker <- get_config_value(config_input, defined_col, "postfix_marker")
  if (!is.null(postfix_marker)) {
    hander_data[, index] <- unname(sapply(hander_data[, index], function(x) {
      if (!str_detect(x, paste0(postfix_marker, "$"))) {
        return(paste0(postfix_marker, x))
      } else {
        return(x)
      }
    }))
  }
  return(hander_data)
}

# lower
handler_lower <- function(hander_data, config_input, defined_col, index, extra_params = list(lower_flag = TRUE)) {
  flag <- extra_params$lower_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  lower <- get_config_value(config_input, defined_col, "lower")
  if (!is.null(lower)) {
    hander_data[, index] <- tolower(hander_data[, index])
  }
  return(hander_data)
}

# upper
handler_upper <- function(hander_data, config_input, defined_col, index, extra_params = list(upper_flag = TRUE)) {
  flag <- extra_params$upper_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  upper <- get_config_value(config_input, defined_col, "upper")
  if (!is.null(upper)) {
    hander_data[, index] <- tolower(hander_data[, index])
  }
  return(hander_data)
}

# str_replace
handler_replace <- function(hander_data, config_input, defined_col, index, extra_params = list(upper_flag = TRUE)) {
  flag <- extra_params$upper_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  replace_pattern <- get_config_value(config_input, defined_col, "replace_pattern")
  replace_string <- get_config_value(config_input, defined_col, "replace_string")
  for (i in 1:length(replace_pattern)) {
    if (!is.null(replace_pattern[i])) {
      hander_data[, index] <- str_replace(hander_data[, index], replace_pattern[i], replace_string[i])
    }
  }
  return(hander_data)
}

# default value
handler_default_value <- function(hander_data, config_input, defined_col, index, extra_params = list(default_value_flag = TRUE)) {
  flag <- extra_params$default_value_flag
  if (!is.null(flag) && !flag) {
    return(hander_data)
  }
  default_value <- get_config_value(config_input, defined_col, "default_value")
  if (!is.null(default_value)) {
    
    hander_data[, index] <- default_value
  }
  return(hander_data)
}

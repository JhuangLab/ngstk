#' Function to show all avaliabe handler function
#' 
#' @param handler_lib handler lib name
#' @param show_all_funs Default is TRUE and to show all functions in the handler_lib
#' @param show_code Default is NULL, select handler you want to see its source code
#' @param show_description Default is FALSE
#' @param handler_confg_file ngstk handler configuration file path, default is
#' system.file('extdata', 'config/handler.toml', package = 'ngstk')
#' @return 
#' NULL
#' @export
#' @examples 
#' show_handlers(show_description = TRUE)
#' show_handlers(show_description = FALSE, show_all_funs = FALSE, 
#'             show_code = 'handler_na_replace')
show_handlers <- function(handler_lib = "default_handlers", show_all_funs = TRUE, 
  show_code = NULL, show_description = FALSE, handler_confg_file = system.file("extdata", 
    "config/handler.toml", package = "ngstk")) {
  handler_lib_data <- eval.config(value = handler_lib, config = "handler", file = handler_confg_file)
  if (show_all_funs) {
    handler_funs <- paste0(handler_lib_data[["handler_funs"]], collapse = ", ")
    msg <- sprintf("%s were exists in file %s %s.", handler_funs, handler_confg_file, 
      handler_lib)
    message(paste0("[INFO] ", msg))
  }
  if (show_description) {
    description <- handler_lib_data$description
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
#' @param mhandler_lib handler lib name 
#' @param show_all_funs Default is TRUE and to show all functions in the handler_lib
#' @param show_code Default is NULL, select handler you want to see its source code
#' @param show_description Default is FALSE
#' @param mhandler_confg_file ngstk handler configuration file path, default is 
#' system.file('extdata', 'config/handler.toml', package = 'ngstk')
#' @return 
#' NULL
#' @export
#' @examples 
#' show_mhandlers(show_description = TRUE)
#' show_mhandlers(show_description = FALSE, show_all_funs = FALSE, 
#'              show_code = 'mhandler_fusions_anyfull_match')
show_mhandlers <- function(mhandler_lib = "default_mhandlers", show_all_funs = TRUE, 
  show_code = NULL, show_description = FALSE, mhandler_confg_file = system.file("extdata", 
    "config/mhandler.toml", package = "ngstk")) {
  mhandler_lib_data <- eval.config(value = mhandler_lib, config = "mhandler", file = mhandler_confg_file)
  if (show_all_funs) {
    mhandler_funs <- paste0(mhandler_lib_data[["mhandler_funs"]], collapse = ", ")
    msg <- sprintf("%s were exists in file %s %s.", mhandler_funs, mhandler_confg_file, 
      mhandler_lib)
    message(paste0("[INFO] ", msg))
  }
  if (show_description) {
    description <- mhandler_lib_data$description
    message(description)
  }
  if (!is.null(show_code)) {
    for (i in show_code) {
      print(eval(parse(text = i)))
    }
  }
}

# Function that can be uesd to process data
handler <- function(handler_data, config_input, defined_cols, input_data, index, 
  handler_funs = NULL, extra_params = NULL) {
  defined_col <- defined_cols[index]
  valid_col_index <- get_valid_col_index(config_input, defined_cols[index], input_data)[1]
  old_op <- options()
  options(stringsAsFactors = F)
  if (is.na(valid_col_index)) {
    empty.col <- rep("NA", nrow(handler_data))
    handler_data <- cbind(handler_data, empty.col)
    warning(sprintf("%s were not exists or not be recognize correctly in input data!", 
      defined_col))
  } else {
    handler_data <- cbind(handler_data, input_data[[valid_col_index]])
  }
  handler_data <- as.data.frame(handler_data)
  colnames(handler_data)[index] <- defined_col
  if (!is.null(handler_funs)) {
    for (i in handler_funs) {
      handler_data <- do.call(i, list(handler_data = handler_data, config_input = config_input, 
        defined_col = defined_col, index = index, extra_params = extra_params))
    }
    
  }
  options(old_op)
  return(handler_data)
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
handler_na_replace <- function(handler_data, config_input, defined_col, index, extra_params = list(na_replace = NULL, 
  na_replace_flag = TRUE)) {
  flag <- extra_params$na_replace_flag
  na_replace <- extra_params$na_replace
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(na_replace)) {
    na_replace <- get_config_value(config_input, defined_col, "na_replace")
  }
  if (!is.null(na_replace)) {
    index <- is.na(handler_data[, index])
    if (any(index)) {
      handler_data[index, index] <- na_replace
    }
  }
  return(handler_data)
}

# map old value to new value
handler_value_map <- function(handler_data, config_input, defined_col, index, extra_params = list(raw_value = NULL, 
  new_value = NULL, value_map_flag = TRUE)) {
  flag <- extra_params$value_map_flag
  raw_value <- extra_params$raw_value
  new_value <- extra_params$new_value
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(raw_value)) {
    raw_value <- get_config_value(config_input, defined_col, "raw")
    new_value <- get_config_value(config_input, defined_col, "new")
  }
  if (!is.null(raw_value)) {
    for (x in 1:length(raw_value)) {
      handler_data[, index] <- str_replace(handler_data[, index], raw_value[x], 
        new_value[x])
    }
  }
  return(handler_data)
}

# use str_extract get value
handler_extract <- function(handler_data, config_input, defined_col, index, extra_params = list(extract_pattern = NULL, 
  extract_flag = TRUE)) {
  flag <- extra_params$extract_flag
  extract_pattern <- extra_params$extract_pattern
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  # Process extract
  if (is.null(extract_pattern)) {
    extract_pattern <- get_config_value(config_input, defined_col, "extract_pattern")
  }
  for (i in extract_pattern) {
    if (!is.null(i)) {
      handler_data[, index] <- str_extract(handler_data[, index], i)
    }
  }
  
  return(handler_data)
}


# use str_split get value
handler_split <- function(handler_data, config_input, defined_col, index, extra_params = list(split_marker = NULL, 
  split_index = NULL, split_flag = TRUE)) {
  flag <- extra_params$split_flag
  split_marker <- extra_params$split_marker
  split_index <- extra_params$split_index
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(split_marker)) {
    split_marker <- get_config_value(config_input, defined_col, "split_marker")
    split_index <- get_config_value(config_input, defined_col, "split_index")
  }
  split_index <- as.numeric(split_index)
  if (!is.null(split_marker)) {
    handler_data[, index] <- sapply(str_split(handler_data[, index], split_marker), 
      function(x) return(x[split_index]))
  }
  return(handler_data)
}

# add prefix
handler_prefix <- function(handler_data, config_input, defined_col, index, extra_params = list(prefix_marker = NULL, 
  prefix_flag = TRUE)) {
  flag <- extra_params$prefix_flag
  prefix_marker <- extra_params$prefix_marker
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(prefix_marker)) {
    prefix_marker <- get_config_value(config_input, defined_col, "prefix_marker")
  }
  if (!is.null(prefix_marker)) {
    handler_data[, index] <- unname(sapply(handler_data[, index], function(x) {
      if (!str_detect(x, paste0("^", prefix_marker))) {
        return(paste0(prefix_marker, x))
      } else {
        return(x)
      }
    }))
  }
  return(handler_data)
}

# add postfix
handler_postfix <- function(handler_data, config_input, defined_col, index, extra_params = list(postfix_marker = NULL, 
  postfix_flag = TRUE)) {
  flag <- extra_params$postfix_flag
  postfix_marker <- extra_params$postfix_marker
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(postfix_marker)) {
    postfix_marker <- get_config_value(config_input, defined_col, "postfix_marker")
  }
  if (!is.null(postfix_marker)) {
    handler_data[, index] <- unname(sapply(handler_data[, index], function(x) {
      if (!str_detect(x, paste0(postfix_marker, "$"))) {
        return(paste0(postfix_marker, x))
      } else {
        return(x)
      }
    }))
  }
  return(handler_data)
}

# lower
handler_lower <- function(handler_data, config_input, defined_col, index, extra_params = list(lower = NULL, 
  lower_flag = TRUE)) {
  flag <- extra_params$lower_flag
  lower <- extra_params$lower
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(lower)) {
    lower <- get_config_value(config_input, defined_col, "lower")
  }
  if (!is.null(lower)) {
    handler_data[, index] <- tolower(handler_data[, index])
  }
  return(handler_data)
}

# upper
handler_upper <- function(handler_data, config_input, defined_col, index, extra_params = list(upper = NULL, 
  upper_flag = TRUE)) {
  flag <- extra_params$upper_flag
  upper <- extra_params$upper
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(upper)) {
    upper <- get_config_value(config_input, defined_col, "upper")
  }
  if (!is.null(upper)) {
    handler_data[, index] <- tolower(handler_data[, index])
  }
  return(handler_data)
}

# str_replace
handler_replace <- function(handler_data, config_input, defined_col, index, extra_params = list(replace_pattern = NULL, 
  replace_string = NULL, replace_flag = TRUE)) {
  flag <- extra_params$replace_flag
  replace_pattern <- extra_params$replace_pattern
  replace_string <- extra_params$replace_string
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(replace_pattern)) {
    replace_pattern <- get_config_value(config_input, defined_col, "replace_pattern")
    replace_string <- get_config_value(config_input, defined_col, "replace_string")
  }
  for (i in 1:length(replace_pattern)) {
    if (!is.null(replace_pattern[i])) {
      handler_data[, index] <- str_replace(handler_data[, index], replace_pattern[i], 
        replace_string[i])
    }
  }
  return(handler_data)
}

# default value
handler_default_value <- function(handler_data, config_input, defined_col, index, 
  extra_params = list(default_value = NULL, default_value_flag = TRUE)) {
  flag <- extra_params$default_value_flag
  default_value <- extra_params$default_value
  if (!is.null(flag) && !flag) {
    return(handler_data)
  }
  if (is.null(default_value)) {
    default_value <- get_config_value(config_input, defined_col, "default_value")
  }
  if (!is.null(default_value)) {
    handler_data[, index] <- default_value
  }
  return(handler_data)
}

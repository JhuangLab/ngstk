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
  valid_cols <- tolower(valid_cols)
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
  if (!is.null(extract_pattern)) {
    hander_data[, index] <- str_extract(hander_data[, index], extract_pattern)
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

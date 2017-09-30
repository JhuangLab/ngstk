# Function that can be uesd to process data
mhandler <- function(hander_data, config_input, mhander_funs = NULL, extra_params = NULL) {
  old_op <- options()
  options(stringsAsFactors = F)
  if (!is.null(mhander_funs)) {
    for (i in mhander_funs) {
      hander_data <- do.call(i, list(hander_data = hander_data, 
                                     config_input = config_input, extra_params = extra_params))
    }
    
  }
  options(old_op)
  return(hander_data)
}

# two colum match
mhander_fusions_left_match <- function(hander_data, config_input, 
                                       extra_params = list(gene_5 = 1, left_gene = NULL, 
                                                           left_match = TRUE)) {
  left_gene <- extra_params$left_gene
  gene_5 <- extra_params$gene_5
  left_match <- extra_params$left_match
  if (!is.null(left_gene) && !is.null(left_match) && left_match) {
    index <- str_detect(hander_data[, gene_5], sprintf("^%s$", left_gene))
    return(hander_data[index, ])
  } else {
    return(hander_data)
  }
}

mhander_fusions_right_match <- function(hander_data, config_input, 
                                        extra_params = list(gene_3 = 2, right_gene = NULL, 
                                                            right_match = TRUE)) {
  right_gene <- extra_params$right_gene
  gene_3 <- extra_params$gene_3
  right_match <- extra_params$right_match
  if (!is.null(right_gene) && !is.null(right_match) && right_match ) {
    index <- str_detect(hander_data[, gene_3], sprintf("^%s$", right_gene))
    return(hander_data[index, ])
  } else {
    return(hander_data)
  }
}

mhander_fusions_any_match <- function(hander_data, config_input, 
                                      extra_params = list(gene_5 = 1, gene_3 = 2, 
  any_gene = NULL, any_match = TRUE)) {
  any_gene <- extra_params$any_gene
  gene_5 <- extra_params$gene_5
  gene_3 <- extra_params$gene_3
  any_match <- extra_params$any_match
  if (!is.null(any_gene) && !is.null(any_match) && any_match ) {
    index_1 <- str_detect(hander_data[, gene_5], sprintf("^%s$", any_gene))
    index_2 <- str_detect(hander_data[, gene_3], sprintf("^%s$", any_gene))
    return(hander_data[index_1 | index_2, ])
  } else {
    return(hander_data)
  }
}

mhander_fusions_full_match <- function(hander_data, config_input, extra_params = list(gene_5 = 1, gene_3 = 2, 
  left_gene = NULL, right_gene = NULL, full_match = TRUE)) {
  left_gene <- extra_params$left_gene
  right_gene <- extra_params$right_gene
  full_match <- extra_params$full_match
  gene_5 <- extra_params$gene_5
  gene_3 <- extra_params$gene_3
  if (!is.null(left_gene) && !is.null(right_gene) && !is.null(full_match) && full_match) {
    index_1 <- str_detect(hander_data[, gene_5], sprintf("^%s$", left_gene))
    index_2 <- str_detect(hander_data[, gene_3], sprintf("^%s$", right_gene))
    return(hander_data[index_1 & index_2, ])
  } else {
    return(hander_data)
  }
}

mhander_fusions_anyfull_match <- function(hander_data, config_input, 
                                          extra_params = list(gene_5 = 1, gene_3 = 2, 
                                          left_gene = NULL, right_gene = NULL,
                                          anyfull_match = TRUE)) {
  left_gene <- extra_params$left_gene
  right_gene <- extra_params$right_gene
  gene_5 <- extra_params$gene_5
  gene_3 <- extra_params$gene_3
  anyfull_match <- extra_params$anyfull_match
  if (!is.null(left_gene) && !is.null(right_gene) && !is.null(anyfull_match) && anyfull_match) {
    index_1 <- str_detect(hander_data[, gene_5], sprintf("^%s$", left_gene))
    index_2 <- str_detect(hander_data[, gene_3], sprintf("^%s$", right_gene))
    
    index_3 <- str_detect(hander_data[, gene_5], sprintf("^%s$", right_gene))
    index_4 <- str_detect(hander_data[, gene_3], sprintf("^%s$", left_gene))
    return(hander_data[(index_1 & index_2) | (index_3 & index_4), ])
  } else {
    return(hander_data)
  }
}

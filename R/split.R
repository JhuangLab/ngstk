#' Data split function by row
#'
#' @param x Data.frame or data.table object that need to be divided n sections by row
#' @param sections Split section number (row)
#' @export
#' @examples
#' x1 <- data.frame(col1 = 1:10, col2 = 11:20)
#' x2 <- data.frame(col1 = 1:99, col2 = 101:199)
#' x <- split_row_data(x1, sections = 1)
#' x <- split_row_data(x1, sections = 2)
#' x <- split_row_data(x1, sections = 3)
#' x <- split_row_data(x1, sections = 4)
#' x <- split_row_data(x2, sections = 2)
#' x <- split_row_data(x2, sections = 3)
split_row_data <- function(x, sections = 1) {
  x_nrow <- nrow(x)
  x_ncol <- ncol(x)
  if (x_nrow < sections) {
    stop('sections must lower than row number of x!')
  }
  regions <- get_split_seqs(x_nrow, sections)
  result <- lapply(1:length(regions), function(i){return(x[regions[[i]],])})
  return(result)
}

#' Data split function by colum
#'
#' @param x Data.frame or data.table object that need to be divided n sections by colum
#' @param sections Split section number (colnum)
#' @export
#' @examples
#' x1 <- data.frame(col1 = 1:10, col2 = 11:20)
#' x1.t <- t(x1)
#' x <- split_col_data(x1.t, sections = 3)
split_col_data <- function(x, sections = 1) {
  x_ncol <- ncol(x)
  if (x_ncol < sections) {
    stop('sections must lower than colum number of x!')
  }
  regions <- get_split_seqs(x_ncol, sections)
  result <- lapply(1:length(regions), function(i){seqs <- regions[[i]];return(x[,seqs])})
  return(result)
}

#' Function to calculate the split regions by sections and total numbers
#'
#' @param total_num Total numbers need to be divided into n sections
#' @param sections Split section number (colnum)
#' @export
#' @examples
#' total_num <- 1000
#' sections <- 3
#' get_split_seqs(total_num, sections)
get_split_seqs <- function(total_num, sections) {
  split_bin <- floor(total_num / sections)
  node_start <- seq(1, total_num, split_bin)
  if ((total_num %% sections) != 0) {
    node_start <- node_start[1:(length(node_start)-1)]
  }
  if (length(node_start) > 1) {
    node_end <- c(node_start[2:length(node_start)] - 1, total_num)
  } else {
    node_start <- 1
    node_end <- total_num
  }
  split_region <- data.frame(node_start, node_end)
  regions <- apply(split_region, 1, function(i){i[1]:i[2]})
  if (typeof(regions) == "integer") {
    regions <- as.data.frame(regions)
    regions <- as.list(regions)
    names(regions) <- NULL
  }
  return(regions)
}

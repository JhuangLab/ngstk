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
    stop("sections must lower than row number of x!")
  }
  regions <- get_split_seqs(x_nrow, sections)
  result <- lapply(1:length(regions), function(i) {
    return(x[regions[[i]], ])
  })
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
    stop("sections must lower than colum number of x!")
  }
  regions <- get_split_seqs(x_ncol, sections)
  result <- lapply(1:length(regions), function(i) {
    seqs <- regions[[i]]
    return(x[, seqs])
  })
  return(result)
}

#' Function to split big file to a series small files (by row)
#' @param filename Filename that need to be split
#' @param each_file_lines Each file row num
#' @param use_system_split Wheather use system split commend
#' @param system_split_params When use_system_split, provide the prefix and other params
#' default is 'split'
#' @param write_fun Function to read data, default is read.table
#' @param write_params_x Parameter name of output object in read.fun
#' @param write_params_file Parameter name of input file in read.fun
#' @param write_params Other parameters pass to write_fun
#' @export
#' @examples
#' dat <- data.frame(col1 = 1:1000)
#' outfn <- tempfile()
#' write.table(dat, outfn, sep = '\t', quote = FALSE, row.names = FALSE)
#' split_row_file(outfn)
split_row_file <- function(filename, each_file_lines = 100, use_system_split = FALSE, 
  system_split_params = "_split", write_fun = "write.table", write_params_x = "x", 
  write_params_file = "file", write_params = list(sep = "", row.names = FALSE, 
    col.names = FALSE, quote = FALSE)) {
  if (use_system_split) {
    split_path <- Sys.which("split")
    split_path <- unname(split_path)
    if (split_path == "") {
      stop("Can't found executable file: split")
    }
    cmd <- sprintf("%s -l %s %s %s", split_path, each_file_lines, filename, paste0(filename, 
      system_split_params))
    cat(cmd, sep = "\n")
    system(cmd)
  }
  fn <- file(filename, "r")
  i <- 1
  pool <- "x"
  while (TRUE) {
    assign(pool[1], value = readLines(fn, n = each_file_lines))
    x <- get(pool[1])
    file <- paste(filename, "split", i, sep = "_")
    config.list.merge(list(x = x, file = file))
    write_params <- eval(parse(text = sprintf("config.list.merge(write_params, list(%s=x, %s=file))", 
      write_params_x, write_params_file)))
    do.call(write_fun, write_params)
    if (length(get(pool[1])) < each_file_lines) {
      break
    } else {
      i <- i + 1
    }
  }
  close(fn)
  return(i)
}

#' Function to split list
#' @param x List object that need to be divided n sections
#' @param sections Split section number (row)
#' @export
#' @examples
#' x <- list(a=1:3, b=2:4, c=3, d=4)
#' split_list(x, 2)
split_list <- function(x, sections = 1) {
  sections <- ceiling(length(x[[1]])/sections)
  a <- seq(from = 1, to = length(x[[1]]), by = sections)
  b <- seq(from = sections, to = length(x[[1]]), by = sections)
  if (length(b) < length(a)) {
    b <- c(b, length(x[[1]]))
  }
  result <- NULL
  for (i in 1:length(a)) {
    result[[i]] <- lapply(x, function(x) x[a[i]:b[i]])
  }
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
  split_bin <- floor(total_num/sections)
  node_start <- seq(1, total_num, split_bin)
  if ((total_num%%sections) != 0) {
    node_start <- node_start[1:(length(node_start) - 1)]
  }
  if (length(node_start) > 1) {
    node_end <- c(node_start[2:length(node_start)] - 1, total_num)
  } else {
    node_start <- 1
    node_end <- total_num
  }
  split_region <- data.frame(node_start, node_end)
  regions <- apply(split_region, 1, function(i) {
    i[1]:i[2]
  })
  if (typeof(regions) == "integer") {
    regions <- as.data.frame(regions)
    regions <- as.list(regions)
    names(regions) <- NULL
  }
  return(regions)
}


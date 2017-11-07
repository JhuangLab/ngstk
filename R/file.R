#' Util function to merge multiple table files.
#' 
#' @param input_files Basename of files that need to be merged, default is NULL 
#' and use the regular expression pattern to select files
#' @param files_dir Directory name of input files
#' @param pattern Use regular expression to select files in files_dir
#' @param outfn Output file path, default is NULL and return the data.frame object
#' @param add.filename Wheather to add the merged filename, default is TRUE
#' @param read_fun Function to read data, default is read.table
#' @param read_params_file Parameter name of input file in read_fun
#' @param read_params Other parameters pass to read_fun
#' @param write_fun Function to read data, default is read.table
#' @param write_params_x Parameter name of output object in read.fun
#' @param write_params_file Parameter name of input file in read.fun
#' @param write_params Other parameters pass to write_fun
#' @param op, Extra option that only take effect in merge process
#' @export
#' @examples 
#' a <- data.frame(col1=1:6, col2=2:7)
#' b <- data.frame(col1=6:11, col2=1:6)
#' file_a <- paste0(tempfile(), '_abcd')
#' file_b <- paste0(tempfile(), '_abcd')
#' write.table(a, file_a, sep = '\t', row.names = FALSE)
#' write.table(b, file_b, sep = '\t', row.names = FALSE)
#' input_files <- c(file_a, file_b)
#' x1 <- merge_table_files(input_files = input_files)
#' x2 <- merge_table_files(files_dir = tempdir(), pattern = '.*_abcd$')
#' outfn = tempfile()
#  x3 <- merge_table_files(files_dir = tempdir(), pattern = ".*_abcd$", outfn = outfn)
merge_table_files <- function(input_files = NULL, files_dir = NULL, pattern = ".*.txt", outfn = NULL, add.filename = TRUE, 
  read_fun = "read.table", read_params_file = "file", read_params = list(sep = "\t", header = TRUE), write_fun = "write.table", 
  write_params_x = "x", write_params_file = "file", write_params = list(sep = "\t", row.names = FALSE), 
  op = list(stringsAsFactors = FALSE)) {
  old_options <- options()
  do.call(options, op)
  
  if (!is.null(files_dir)) {
    input_files_tmp <- list.files(files_dir, pattern = pattern)
    input_files <- c(input_files, input_files_tmp)
  }
  if (is.null(input_files)) {
    return(NULL)
  }
  
  result <- NULL
  for (i in input_files) {
    if (!is.null(files_dir)) {
      input_file <- sprintf("%s/%s", files_dir, i)
    } else {
      input_file <- i
    }
    if (!file.exists(input_file)) {
      next
    }
    read_params <- eval(parse(text = sprintf("config.list.merge(read_params, list(%s=input_file))", read_params_file)))
    result.tmp <- do.call(read_fun, read_params)
    if (add.filename) {
      filename = input_file
      result.tmp <- cbind(filename, result.tmp)
    }
    result <- rbind(result, result.tmp)
  }
  if (is.null(outfn)) {
    return(result)
  } else {
    write_params <- eval(parse(text = sprintf("config.list.merge(write_params, list(%s=result, %s=outfn))", write_params_x, 
      write_params_file)))
    status <- do.call(write_fun, write_params)
  }
  options(old_options)
  return(status)
}

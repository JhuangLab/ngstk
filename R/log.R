#' Function to check file last change time and 
#' according the requirement to return check value
#' 
#' @param input_files Basename of files that need to be check, default is NULL 
#' and use the regular expression pattern to select files
#' @param files_dir Directory name of input files
#' @param pattern Use regular expression to select files in files_dir
#' @param return_mtime Logical indicating wheather to return files modification times
#' @param return_check Logical indicating wheather to return file times check status
#' @param check_time_fun Function to check files time, default is all equal
#' @export
#' @examples 
#' file_a <- tempfile()
#' file_b <- tempfile()
#' file.create(c(file_a, file_b))
#' get_files_mtime(input_files = c(file_a, file_b), return_mtime = TRUE)
get_files_mtime <- function(input_files = NULL, files_dir = NULL, pattern = ".*.txt", 
  return_mtime = TRUE, return_check = TRUE, check_time_fun = function(files_mtime) {
    all(files_mtime == files_mtime[1])
  }) {
  input_files <- get_files(input_files, files_dir, pattern)
  if (is.null(input_files)) {
    return(NULL)
  }
  files_mtime <- file.info(input_files)$mtime
  status <- check_time_fun(files_mtime)
  result <- NULL
  if (return_mtime && return_check) {
    return(list(files_mtime = files_mtime, status = status))
  } else if (return_mtime && !return_check) {
    return(files_mtime)
  } else if (return_check && !return_mtime) {
    return(status)
  } else {
    return(files_mtime)
  }
}
#' Function to check file create time and 
#' according the requirement to return check value
#' 
#' @param input_files Basename of files that need to be check, default is NULL 
#' and use the regular expression pattern to select files
#' @param files_dir Directory name of input files
#' @param pattern Use regular expression to select files in files_dir
#' @param return_ctime Logical indicating wheather to return files modification times
#' @param return_check Logical indicating wheather to return file times check status
#' @param check_time_fun Function to check files time, default is all equal
#' @export
#' @examples 
#' file_a <- tempfile()
#' file_b <- tempfile()
#' file.create(c(file_a, file_b))
#' get_files_ctime(input_files = c(file_a, file_b), return_ctime = TRUE)
get_files_ctime <- function(input_files = NULL, files_dir = NULL, pattern = ".*.txt", 
  return_ctime = TRUE, return_check = TRUE, check_time_fun = function(files_ctime) {
    all(files_ctime == files_ctime[1])
  }) {
  input_files <- get_files(input_files, files_dir, pattern)
  if (is.null(input_files)) {
    return(NULL)
  }
  files_ctime <- file.info(input_files)$ctime
  status <- check_time_fun(files_ctime)
  result <- NULL
  if (return_ctime && return_check) {
    return(list(files_ctime = files_ctime, status = status))
  } else if (return_ctime && !return_check) {
    return(files_ctime)
  } else if (return_check && !return_ctime) {
    return(status)
  } else {
    return(files_ctime)
  }
}

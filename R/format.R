#' Function to format filenames that can be used to unify the filenames style for 
#' more easily download or use
#' @param input_files Basename of files that need to be format, default is NULL 
#' and use the regular expression pattern to select files
#' @param files_dir Directory name of input files
#' @param pattern Use regular expression to select files in files_dir
#' @param do.rename If set TRUE, it will do rename step
#' @param profix Profix of filenames added in those without the same profix
#' @param prefix Prefix of filenames added in those without the same profix
#' @param replace Use str_replace to replace all old to new separately
#' @export
#' @examples 
#' files_dir <- system.file('extdata', 'demo/format', package = 'ngstk')
#' pattern <- '*.txt'
#' x <- format_filenames(files_dir = files_dir, pattern = pattern, profix = 'hg38_')
format_filenames <- function(input_files = NULL, files_dir = NULL, pattern = ".*.txt", 
  do.rename = FALSE, profix = "", prefix = "", replace = list(old = c("-", "__"), 
    new = c("_", "_"))) {
  
  input_files <- get_files(input_files, files_dir, pattern)
  input_files_dir <- dirname(input_files)
  input_files <- basename(input_files)
  if (is.null(input_files)) {
    return(NULL)
  }
  
  for (i in 1:length(replace$old)) {
    if (i == 1) {
      filenames.new <- sapply(input_files, function(x, o = replace$old[i], 
        n = replace$new[i]) {
        str_replace_all(x, o, n)
      })
    } else {
      filenames.new <- sapply(filenames.new, function(x, o = replace$old[i], 
        n = replace$new[i]) {
        str_replace_all(x, o, n)
      })
    }
  }
  
  filenames.new <- sapply(filenames.new, function(x) {
    if (!str_detect(x, paste0("^", profix))) {
      return(paste0(profix, x))
    } else {
      return(x)
    }
  })
  
  filenames.new <- sapply(filenames.new, function(x) {
    if (!str_detect(x, sprintf("%s$", prefix))) {
      return(paste0(profix, x))
    } else {
      return(x)
    }
  })
  
  for (i in 1:length(replace$old)) {
    filenames.new <- sapply(filenames.new, function(x, old = replace$old[i], 
      new = replace$new[i]) {
      str_replace(x, old, new)
    })
  }
  input_files_dir <- normalizePath(input_files_dir)
  input_files <- sprintf("%s/%s", input_files_dir, input_files)
  filenames.new <- sprintf("%s/%s", input_files_dir, filenames.new)
  if (do.rename) {
    file.rename(input_files, filenames.new)
  } else {
    return(filenames.new)
  }
}

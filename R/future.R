#' Function to download multiple file at the same time.
#'
#' @param urls A character string naming the URLs of a resource 
#' to be downloaded.
#' @param destfiles Default to use the basename of urls
#' @param logdir Directory to store the download logs [tempdir()]
#' @param ids_func Function to define the returned ids
#' @param logfn_func Function to define the log files
#' @param parallel_method Method for future parallel [plan(multiprocess)]
#' @param ... Other params pass to \code{\link{download.file}}
#' @export
#' @return A list
#' @examples
#' urls <- c(paste0('https://raw.githubusercontent.com/',
#' 'Miachol/ftp/master/files/images/bioinstaller/maftools3.png'), 
#' paste0('https://raw.githubusercontent.com/',
#' 'Miachol/ftp/master/files/images/bioinstaller/maftools4.png'))
#' par_download(urls, sprintf('%s/%s', tempdir(), basename(urls)))
par_download <- function(urls, destfiles = NULL, logdir = sprintf("%s/logs", tempdir()), 
  ids_func = function(urls) {
    stri_rand_strings(n = length(urls), length = 20)
  }, logfn_func = function(ids, logdir) {
    sprintf("%s/%s.log", logdir, ids)
  }, parallel_method = "plan(multiprocess)", ...) {
  is_fine <- is.character(urls)
  if (!all(is_fine)) 
    warning(sprintf("%s is not character.", paste0(urls[!is_fine], collapse = ", ")))
  if (is.null(destfiles)) 
    destfiles <- basename(urls)
  if (length(destfiles) != length(urls)) 
    stop("Please keep the same nums: urls and destfiles")
  eval(parse(text = parallel_method))
  ids <- do.call(ids_func, list(urls = urls))
  logs <- do.call(logfn_func, list(ids = ids, logdir = logdir))
  if (!dir.exists(logdir)) 
    dir.create(logdir, recursive = TRUE)
  file.create(logs)
  msg <- sprintf("Downloading %s (log id: %s) destfile is %s. Log saved to %s.", 
    urls, ids, destfiles, logs)
  sapply(msg, message)
  flush(stderr())
  status <- list()
  for (i in 1:length(urls)) {
    status[[ids[i]]] <- future({
      con <- file(logs[i], "w")
      sink(file = con, type = "message")
      x <- download.file(urls[i], destfiles[i], ...)
      sink(file = con)
      return(x)
    })
  }
  status <- lapply(status, FUN = value)
  if (length(grep("makeCluster", parallel_method)) > 0) {
    x <- str_split(parallel_method, " =| <-")[[1]][1]
    x <- str_replace_all(x, " ,", "")
    do.call(stopCluster, list(x))
  }
  return(status)
}

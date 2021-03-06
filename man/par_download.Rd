% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/future.R
\name{par_download}
\alias{par_download}
\title{Function to download multiple file at the same time.}
\usage{
par_download(urls, destfiles = NULL, logdir = sprintf("\%s/logs",
  tempdir()), ids_func = function(urls) {     stri_rand_strings(n =
  length(urls), length = 20) }, logfn_func = function(ids, logdir) {    
  normalizePath(sprintf("\%s/\%s.log", logdir, ids), mustWork = FALSE) },
  parallel_method = "plan(multiprocess)", ...)
}
\arguments{
\item{urls}{A character string naming the URLs of a resource 
to be downloaded.}

\item{destfiles}{Default to use the basename of urls}

\item{logdir}{Directory to store the download logs [tempdir()]}

\item{ids_func}{Function to define the returned ids}

\item{logfn_func}{Function to define the log files}

\item{parallel_method}{Method for future parallel [plan(multiprocess)]}

\item{...}{Other params pass to \code{\link{download.file}}}
}
\value{
A list
}
\description{
Function to download multiple file at the same time.
}
\examples{
urls <- c(paste0('https://raw.githubusercontent.com/',
'Miachol/ftp/master/files/images/bioinstaller/maftools3.png'), 
paste0('https://raw.githubusercontent.com/',
'Miachol/ftp/master/files/images/bioinstaller/maftools4.png'))
\dontrun{
par_download(urls, sprintf('\%s/\%s', tempdir(), basename(urls)))
}
}

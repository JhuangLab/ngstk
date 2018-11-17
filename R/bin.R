#' Function to generate executable files for R package
#'
#' @param pkgs Names f R packages to generate executable files
#' @param destdir Destination directory to store the inst/bin files [~/.ngstk/bin] 
#' @param ... Params pass to \link{file.rename}.
#' @export
#' @examples
#' rbin('ngstk', tempdir())
rbin <- function(pkgs, destdir = "~/.ngstk/bin") {
  destdir <- normalizePath(destdir, mustWork = FALSE)
  if (!dir.exists(destdir)) dir.create(destdir, recursive = TRUE)
  status <- sapply(pkgs, function(x) {
    tmp_dir <- sprintf("%s/%s", tempdir(), stri_rand_strings(n = 1, length = 20))
    dir.create(tmp_dir)
    files <- list.files(system.file('extdata', 'bin', package = x), '.*', full.names = TRUE, 
                        recursive = TRUE)
    fn_bname <- basename(files)
    status <- file.copy(files, sprintf("%s/%s", tmp_dir, fn_bname))
    failed_fn <- NULL
    if (!all(status)) {failed_fn <- files[status]; file.copy(failed_fn, sprintf("%s/%s_%s", tmp_dir, x, failed_fn))}

    files <- list.files(tmp_dir, ".*")
    fn_bname <- basename(files)
    Sys.chmod(sprintf("%s/%s", tmp_dir, fn_bname), "775")
    message(sprintf("Copying %s bin/ %s to %s", x, fn_bname, destdir))
    status <- file.copy(sprintf("%s/%s", tmp_dir, fn_bname), sprintf("%s/", destdir), overwrite = TRUE)
    names(status) <- fn_bname
    return(status)
  })
  if (length(grep(destdir, Sys.getenv("PATH"))) == 0) {
    message(sprintf("Please set %s in your PATH to use the bin files.", destdir))
    message(sprintf("Linux/Mac OS X: echo 'export PATH=$PATH:%s\\n' >> ~/.bashrc", destdir))
    message(sprintf("R users: echo 'Sys.setenv(PATH=\"%s\")\\n' >> ~/.Rprofile", paste(Sys.getenv("PATH"), destdir, sep = ":")))
  }
  return(status)
}

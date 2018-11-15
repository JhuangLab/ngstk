#' Function to generate time stamp in the files name or directories name.
#'
#' @param template The template time format with '_'
#' @param extra_flag Using extra flag to replace template '_' and get extra time stamp ['-', '-']
#' @export
#' @return A list
#' @examples
#' time_stamp()
time_stamp <- function(template = c("%Y_%m_%d_%H_%M_%S_", "%Y_%m_%d_%H_%M_", "%Y_%m_%d_%H_", 
  "%Y_%m_%d_", "%Y_%m_", "%Y_"), extra_flag = c("-", "/", "@")) {
  availabe_time_stamp <- list()
  availabe_time_stamp[[1]] <- template
  for (i in 1:length(extra_flag)) {
    availabe_time_stamp[[i + 1]] <- str_replace_all(availabe_time_stamp[[1]], 
      "_", extra_flag[i])
  }
  stamps <- lapply(availabe_time_stamp, function(x) format(Sys.time(), x))
  return(stamps)
}

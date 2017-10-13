
# add location
addLoc <- function(map_config, rect_config) {
  group_num_each <- rep(0, map_config$group_num)
  leg_num_each <- rep(0, map_config$group_num)

  for (i in 1:map_config$group_num) {
    group_name <- paste("group_", i, sep = "")
    group_info <- map_config[[which(names(map_config) == group_name)]]
    group_type <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[1]]))
    group_color_theme <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[2]]))
    group_col_num <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[3]]))
    group_num_each[i] <- length(group_col_num)
    leg_num_each[i] <- length(group_type)
  }

  sample_loc_info <- getHref(100, 50 + sum(group_num_each)*rect_config$height + rect_config$span*2 , "sample_name_list")
  loc_info <- lapply(1:length(group_num_each), function(x) {
    return(paste(getHref(100, 50 + sum(group_num_each[1:x-1])*rect_config$height + rect_config$span*(x-1) , paste("group_", x, sep = "")),
                 getHref(100, 50 + sum(group_num_each[1:x-1])*rect_config$height + rect_config$span*(x-1) , paste("group_", x, "_outline", sep = "")),
                 getHref(100, 80 + 50 + sum(group_num_each)*rect_config$height + rect_config$span*5 + sum(leg_num_each[1:x-1])*rect_config$height*2.5, paste("group_", x, "_leg", sep = "")),
                 sep = "\n"))

  })

  loc_info_total <- paste(sample_loc_info, paste(loc_info, collapse = "\n"), sep = "\n")
  return(loc_info_total)
}

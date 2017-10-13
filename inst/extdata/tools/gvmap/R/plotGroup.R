
# plot group
plotGroup <- function(data_info, map_config, color_config, rect_config, i) {
  group_name <- paste("group_", i, sep = "")
  group_name_leg <- paste("group_", i, "_leg", sep = "")
  group_info <- map_config[[which(names(map_config) == group_name)]]
  group_type <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[1]]))
  group_color_theme <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[2]]))
  group_col_num <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[3]]))

  # rect information
  baseline <- 0
  rect <- rep("", length(group_type))
  leg_text <- rep("", length(group_type))
  for (n in 1:length(group_type)) {
    type <- group_type[n]
    color_theme <- group_color_theme[n]
    col_num <- group_col_num[n]
    rect[n] <- getRowRectSVG(type, color_theme, col_num, data_info, color_config, rect_config, n-1)
    leg_text[n] <- getRowLegendText(type, color_theme, col_num, data_info, color_config, rect_config, n-1)
  }

  if (rect_config$frame) {
    group_outline_g <- paste("group_", i, "_outline", sep = "")
    gourp_outline <- getRect(0, 0, rect_config$width * length(data_info[, 1]), rect_config$height * length(group_col_num),
                             "none", color_config$black_col, 1)
    if (!is.null(map_config$split_sample)) {
      sample_list <- data_info[, map_config$sample_col_num]
      sample_line <- lapply(1:length(map_config$split_sample), function(x) {
        sample_index <- which(sample_list == map_config$split_sample[x])
        return(getLine(rect_config$width * sample_index, 0, rect_config$width * sample_index, rect_config$height * length(group_col_num), color_config$black_col, 1))
      })
    } else {
      sample_line <- ""
    }
    group_svg <- paste(SVGgroupHeader(id = group_name), paste(rect, collapse = "\n"), SVGgroupFooter(),
                       SVGgroupHeader(id = group_outline_g), gourp_outline, paste(sample_line, collapse = "\n"), SVGgroupFooter(),
                       SVGgroupHeader(id = group_name_leg), paste(leg_text, collapse = "\n"), SVGgroupFooter(),
                       sep = "\n")
  } else {
    group_svg <- paste(SVGgroupHeader(id = group_name), paste(rect, collapse = "\n"), SVGgroupFooter(),
                       SVGgroupHeader(id = group_name_leg), paste(leg_text, collapse = "\n"), SVGgroupFooter(),
                       sep = "\n")
  }

  return(group_svg)
}


# get legend
getRowLegendText <- function(type, color_theme, col_num, data_info, color_config, rect_config, baseline) {
  content <- as.matrix(data_info[, col_num])
  title <- getText(0, baseline*rect_config$height*2.5, "start", 6, rect_config$font_family, 0, type)
  if (color_theme == "bg_col") {
    #title <- getText(0, baseline*rect_config$height*2.5, "start", rect_config$leg_font_size, rect_config$font_family, 0, type)
    basic_info <- paste(getRect(0, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, color_config$bg_col, "none", 0),
                        getText(0 + rect_config$height, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content[1,1]),
                        sep = "\n")
  } else if (color_theme == "tag_col") {
    content_element <- unique(as.vector(content))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    #title <- getText(0, baseline*rect_config$height*2.5, "start", rect_config$leg_font_size, rect_config$font_family, 0, type)
    basic_info <- paste(getRect(0, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, color_config$tag_col, "none", 0),
                        getText(0 + rect_config$height, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content_element[1]),
                        sep = "\n")
  } else if (color_theme == "mutation_col") {
    content_element <- unique(tolower(unlist(strsplit(as.vector(content), split="/"))))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    basic_info <- lapply(1:length(content_element), function(x) {
      rect_color <- color_config$mutation_col[which(names(color_config$mutation_col) == content_element[x])]
      basic_info_single <- paste(getRect(0 + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, rect_color, "none", 0),
                                 getText(0 + rect_config$height + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content_element[x]),
                                 sep = "\n")
      return(basic_info_single)
    })
  } else if (color_theme == "binary_col") {
    content_element <- unique(as.vector(content))
    content_element <- sort(content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))])
    basic_info <- lapply(1:2, function(x) {
      basic_info_single <- paste(getRect(0 + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, color_config$binary_col[x], "none", 0),
                                 getText(0 + rect_config$height + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content_element[x]),
                                 sep = "\n")
      return(basic_info_single)
    })
  } else if (color_theme == "pool_col") {
    content_element <- unique(as.vector(content))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    basic_info <- lapply(1:length(content_element), function(x) {
      rect_color <- color_config$mutation_col[which(names(color_config$mutation_col) == content_element[x])]
      basic_info_single <- paste(getRect(0 + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, color_config$pool_col[x], "none", 0),
                                 getText(0 + rect_config$height + (x-1)*50, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content_element[x]),
                                 sep = "\n")
      return(basic_info_single)
    })
  } else {
    content_element <- unique(as.vector(content))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    color_user <- unlist(color_config[which(names(color_config) == as.character(color_theme))])[1]
    basic_info <- paste(getRect(0, baseline*rect_config$height*2.5 + rect_config$height*0.5, rect_config$height*0.7, rect_config$height*0.7, color_user, "none", 0),
                        getText(0 + rect_config$height, baseline*rect_config$height*2.5 + rect_config$height, "start", 4, rect_config$font_family, 0, content_element[1]),
                        sep = "\n")
  }
  leg_text <- paste(title, paste(basic_info, collapse = "\n"), sep = "\n")
  return(leg_text)
}


# get every row rect element
getRowRectSVG <- function(type, color_theme, col_num, data_info, color_config, rect_config, baseline) {
  colname_info <- type
  #message(colname_info)
  content <- as.matrix(data_info[, col_num])
  rect <- rep("", length(col_num))

  colname_text <-  paste(getText(0-4, (0.8+baseline)*rect_config$height, "end", rect_config$leg_font_size, rect_config$font_family, 0, colname_info),
                         getLine(0, (0.5+baseline)*rect_config$height, 0-3, (0.5+baseline)*rect_config$height, color_config$black_col, 1),
                         sep = "\n")

  if (color_theme == "bg_col") {
    # bg_col theme
    rect <- lapply(1:length(content), function(x) {
      getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
    })
  } else if (color_theme == "tag_col") {
    # tag_col theme
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
      } else {
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$tag_col, color_config$white_col, rect_config$stroke_width)
      }
    })
  } else if (color_theme == "mutation_col") {
    # mutation_col theme
    mut_colors <- color_config$mutation_col
    rect <- lapply(1:length(content), function(x) {
      #message(x)
      if (content[x] == 0 | is.na(content[x])) {
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
      } else {
        mut_type <- data.frame(table(strsplit(tolower(as.character(content[x])), split="/")))
        if (length(mut_type$Var1) == 1) {
          mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1))])
          if (length(mut_color) == 0) {
            mut_color <- color_config$white_col
          }
          getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, mut_color, color_config$white_col, rect_config$stroke_width)
        } else {
          sub_rect <- rep("", length(mut_type$Var1) + 1)
          sum_freq <- sum(mut_type$Freq)
          for (m in (1:length(mut_type$Var1))) {
            mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1[m]))])
            sub_rect[m] <- getRect((x-1)*rect_config$width, baseline*rect_config$height + (sum(mut_type$Freq[1:m-1])/sum_freq)*rect_config$height,
                                   rect_config$width, rect_config$height*(sum(mut_type$Freq[m])/sum_freq),
                                   mut_color, "none", 0)
          }
          sub_rect[length(mut_type$Var1)+1] <- getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, "none", color_config$white_col, rect_config$stroke_width)
          sub_rect_single <- paste(sub_rect, collapse = "\n")
          return(sub_rect_single)
        }
      }
    })
  } else if (color_theme == "binary_col") {
    content_element <- unique(as.vector(content))
    content_element <- sort(content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))])
    if (length(content_element) != 2) {
      stop("don't match binaly element color theme")
    } else {
      rect <- lapply(1:length(content), function(x) {
        #message(content[x,k])
        if (content[x] == 0 | is.na(content[x])) {
          getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
        } else if (content[x] == content_element[1]) {
          getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$binary_col[1], color_config$white_col, rect_config$stroke_width)
        } else if (content[x] == content_element[2]) {
          getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$binary_col[2], color_config$white_col, rect_config$stroke_width)
        } else {
          getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
        }
      })
    }
  } else if (color_theme == "pool_col") {
    content_element <- unique(as.vector(content))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
      } else {
        tt <- which(content_element == content[x])
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$pool_col[tt], color_config$white_col, rect_config$stroke_width)
      }
    })
  } else {
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_config$bg_col, color_config$white_col, rect_config$stroke_width)
      } else {
        color_user <- unlist(color_config[which(names(color_config) == as.character(color_theme))])[1]
        getRect((x-1)*rect_config$width, baseline*rect_config$height, rect_config$width, rect_config$height, color_user, color_config$white_col, rect_config$stroke_width)
      }
    })
  }
  rect_total <- paste(paste(rect, collapse = "\n"),
                      paste(colname_text, collapse = "\n"),
                      sep = "\n")
  return(rect_total)
}

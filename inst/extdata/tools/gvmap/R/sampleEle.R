
# get sample text SVG element
getSampleEle <- function(data_info, map_config, color_config, rect_config) {
  sample_list <- data_info[, map_config$sample_col_num]
  sample_g <- SVGgroupHeader(id = "sample_name_list")
  sample_s <- lapply(1:length(sample_list), function(x) getText((x-1)*rect_config$width, 5, "start", rect_config$sample_font_size, rect_config$font_family, 90, sample_list[x]))
  sample_svg <- paste(sample_g, paste(sample_s, collapse = "\n"), SVGgroupFooter(), sep = "\n")

  return(sample_svg)
}





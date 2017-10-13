#
# plot of genome data such as fusion and variant
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


gvmap <- function(data_file, config_file, output_svg_name) {
  # check output parameter
  if (file.exists(output_svg_name)) {
    message("**  ", output_svg_name , " already exists, the gvmap result will replace it.")
  }

  # read file
  data_info <- readDataFile(data_file)
  config_info <- readConfig(config_file)

  map_config <- config_info$map_config
  color_config <- config_info$color_config
  rect_config <- config_info$rect_config

  # plot sample name
  sample_svg <- getSampleEle(data_info, map_config, color_config, rect_config)

  # plot group information
  group_svg <- lapply(1:map_config$group_num, function(i) {
    plotGroup(data_info, map_config, color_config, rect_config, i)
  })
  group_svg <- paste(group_svg, collapse = "\n")

  # add location
  loc_svg <- addLoc(map_config, rect_config)

  # output svg file
  output_info <- list(
    sample_svg = sample_svg,
    group_svg = group_svg,
    loc_svg = loc_svg
  )
  outputSVG(output_info, output_svg_name, rect_config)
}

# output SVG result
outputSVG <- function(output_info, output_svg_name, rect_config) {
  write(SVGheader(rect_config$plot_width, rect_config$plot_height), file = output_svg_name, sep = "\n")
  write(SVGdefHeader(), file = output_svg_name, append = T,  sep = "\n")
  write(output_info$sample_svg, file = output_svg_name, append = T,  sep = "\n")
  write(output_info$group_svg, file = output_svg_name, append = T,  sep = "\n")
  write(SVGdefFooter(), file = output_svg_name, append = T,  sep = "\n")
  write(output_info$loc_svg, file = output_svg_name, append = T,  sep = "\n")
  write(SVGfooter(), file = output_svg_name, append = T, sep = "\n")
}


# read data file
readDataFile <- function(data_file) {
  if (typeof(data_file) == "character") {
    data_info <- read.table(data_file, header = T, sep = "\t", quote = '"')
  } else {
    data_info <- data_file
  }

  return(data_info)
}

# read map config file
readConfig <- function(config_file) {
  if (is.yaml.file(config_file)) {
    config_info <- read.config(config_file)
  } else {
    stop("must input a right config file of YAML format")
  }

  return(config_info)
}


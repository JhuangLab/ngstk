

# SVG header decleration from w3c school
SVGheader <- function(width, height) {
  svg_header <- sprintf("<?xml version=\"1.0\" standalone=\"no\"?> \n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> \n\n<svg width=\"%s\" height=\"%s\" version=\"1.1\" \n  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n", width, height)
  return(svg_header)
}

# SVG footer decleration from w3c school
SVGfooter <- function() {
  svg_footer <- sprintf("\n</svg>")
  return(svg_footer)
}

# SVG def tag header decleration from w3c school
SVGdefHeader <- function() {
  svg_def_header <- sprintf("<defs>")
  return(svg_def_header)
}

# SVG def tag footer from w3c school
SVGdefFooter <- function() {
  svg_def_footer <- sprintf("</defs>")
  return(svg_def_footer)
}

# SVG group header
SVGgroupHeader <- function(id) {
  return(sprintf("<g id=\"%s\">", id))
}

# SVG group Footer
SVGgroupFooter <- function() {
  return(sprintf("</g>"))
}

# get rect element
getRect <- function(x, y, width, height, fill, stroke, stroke_width) {
  return(sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" style=\"fill:%s; stroke: %s; stroke-width:%s; opacity:1.0\"/>", x, y, width, height, fill, stroke, stroke_width))
}

# get text element
getText <- function(x, y, anchor, font_size, font_family, transform, content) {
  return(sprintf("<text x=\"%s\" y=\"%s\" style=\"text-anchor: %s;font-size: %spt; font-family:%s;\" transform=\"rotate(%s,%s,%s)\">%s</text>", x, y, anchor, font_size, font_family, transform, x, y, content))
}

# get href
getHref <- function(x, y, id) {
  return(sprintf("<use xlink:href=\"#%s\" x=\"%s\" y=\"%s\" />", id, x, y))
}

# get line
getLine <- function(x1, y1, x2, y2, stroke, stroke_width) {
  return(sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" style=\"stroke:%s; stroke-width:%s\"/>", x1, y1, x2, y2, stroke, stroke_width))
}










#' Function to convert mutation data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A mutation data.frame need to be converted to ProteinPaint input.
#' @param format Point the input data format (annovar or others)
#' @param order Input data cols order (chr, start, end, trascript and aachange)
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' example <- NULL

mut2pp <- function(input_data, format = "annovar", 
                                    order = list(chr = 1, start = 2, end = 3, transcript = 4, aachange = 5)
                                    ) {
  if (format == "annovar") {
    
  }
}


#' Function to convert fusion data to ProteinPaint \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input.
#' @param format Point the input data format (annovar or others)
#' @param order Input data cols order (chr, start, end, trascript and aachange)
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' example <- NULL
fusion2pp <- function(input_data, format = "fusioncatcher", 
                                    order = list(gene5 = 1, gene3 = 2)
) {
  if (format == "fusioncatcher") {
    
  }
}

#' Function to convert fusion data to ProteinPaint heatmap meta rows \url{https://pecan.stjude.org/pp} input format.
#'
#' @param input_data A gene fusions data.frame need to be converted to ProteinPaint input (heatmap meta rows).
#' @param format Point the input data format (annovar or others)
#' @param order Input data cols order (chr, start, end, trascript and aachange)
#' 
#' @return
#' A data frame
#' @export
#' @examples
#' example <- NULL
fusion2pp_meta <- function(input_data, format = "fusioncatcher", 
                      order = list(sample = 1, gene5 = 2, gene3 = 3)) {
  if (format == "fusioncatcher") {
    
  }
}
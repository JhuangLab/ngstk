#' Function to get samplegroup file that can be pass to Proteinpaint
#' 
#' @param samples A vector indicating all samples
#' @param group A vector indicating the group information 
#' @param outfn Default is NULL and not output the result to file
#' 
#' @return 
#' A data frame
#' @export
#' @examples 
#' samples <- sprintf('A%s', 1:7)
#' group <- 'B-ALL'
#' samplegroup <- get_pp_samplegroup(samples, group)
#' outfn <- tempfile()
#' samplegroup <- get_pp_samplegroup(samples, group, outfn)
get_pp_samplegroup <- function(samples, group, outfn = NULL) {
  pp_samplegroup <- data.frame(group, samples)
  if (!is.null(outfn)) {
    write.table(pp_samplegroup, outfn, sep = "\t", row.names = F, quote = F, 
      col.names = F)
  }
  return(pp_samplegroup)
}

#' To format ProteinPaint input meta data of age
#' 
#' @param raw_meta A data.frame contain cols of 'sample', 'term', 
#' 'group', 'value', 'color' and 'legendorder'
#' @param outfn Output file, default is NULL and not output to file
#' @param age_group Name of age group, default is 'Age'
#' @param adult_value Value of adult, default is 'Adult'
#' @param child_value Value of child, default is 'Pediatric'
#' @param adult_color Color of adult, default is '#c20b01'
#' @param child_color Color of child, default is '#196abd'
#' @param ... Parameters pass to \code{\link{set_colors}}
#' @return 
#' A data frame
#' @export
#' @examples 
#' 
#' meta_template <- system.file('extdata', 
#' 'demo/proteinpaint/heatmap_meta_template.txt', 
#' package = 'ngstk')
#' raw_meta <- read.table(meta_template, sep = '\t', header = TRUE)
#' term <- group <- 'Age'
#' raw_meta$term <- term
#' raw_meta$group <- group
#' raw_meta$value <- c(rep(c('Adult', 'Pediatric'), 3),  'Male')
#' meta_age <- format_pp_meta_age(raw_meta)
format_pp_meta_age <- function(raw_meta, outfn = NULL, age_group = "Age", adult_value = "Adult", 
  child_value = "Pediatric", adult_color = "#c20b01", child_color = "#196abd", 
  ...) {
  pp_meta <- format_pp_meta_2_level(raw_meta, outfn, age_group, adult_value, child_value, 
    adult_color, child_color, ...)
  return(pp_meta)
}

#' To format ProteinPaint input meta data of gender
#' 
#' @param raw_meta A data.frame contain cols of 'sample', 
#' 'term', 'group', 'value', 'color' and 'legendorder'
#' @param outfn Output file, default is NULL and not output to file
#' @param gender_group Name of age group, default is 'Gender'
#' @param male_value Value of male, default is 'Male'
#' @param female_value Value of female, default is 'Female'
#' @param male_color Color of male, default is '#c20b01'
#' @param female_color Color of female, default is '#196abd'
#' @param ... Parameters pass to \code{\link{set_colors}}
#' @return 
#' A data frame
#' @export
#' @examples 
#' meta_template <- system.file('extdata', 
#' 'demo/proteinpaint/heatmap_meta_template.txt', package = 'ngstk')
#' raw_meta <- read.table(meta_template, sep = '\t', header = TRUE)
#' term <- group <- 'Gender'
#' raw_meta$term <- term
#' raw_meta$group <- group
#' raw_meta$value <- c(rep(c('Male', 'Female'), 3), 'Male')
#' meta_gender <- format_pp_meta_gender(raw_meta)
format_pp_meta_gender <- function(raw_meta, outfn = NULL, gender_group = "Gender", 
  male_value = "Male", female_value = "Female", male_color = "#c20b01", female_color = "#196abd", 
  ...) {
  pp_meta <- format_pp_meta_2_level(raw_meta, outfn, gender_group, male_value, 
    female_value, male_color, female_color, ...)
  return(pp_meta)
}

format_pp_meta_2_level <- function(raw_meta, outfn = NULL, group = "default", positive_value = "1", 
  negative_value = "0", positive_color = NULL, negative_color = NULL, ...) {
  if (is.null(positive_color)) {
    colors <- set_colors(...)
  } else {
    colors <- c(positive_color, negative_color)
  }
  positive_color <- colors[1]
  negative_color <- colors[2]
  pp_meta <- raw_meta
  pp_meta$color <- ""
  pp_meta$value <- str_replace(pp_meta$value, " ", "")
  pp_meta$legendorder[is.na(pp_meta$legendorder)] <- ""
  group_positive_index <- which(pp_meta$group == group & pp_meta$value == positive_value)[1]
  group_negative_index <- which(pp_meta$group == group & pp_meta$value == negative_value)[1]
  pp_meta$color[group_positive_index] <- positive_color
  pp_meta$color[group_negative_index] <- negative_color
  pp_meta$value <- str_replace(pp_meta$value, "#N/A", "NA")
  pp_meta <- pp_meta[pp_meta$value != "NA", ]
  if (!is.null(outfn)) {
    write.table(pp_meta, outfn, sep = "\t", row.names = F, quote = F)
  }
  return(pp_meta)
}

#' To format ProteinPaint input meta data of gender
#' 
#' @param raw_meta A data.frame contain cols of 'sample', 
#' 'term', 'group', 'value', 'color' and 'legendorder'
#' @param outfn Output file, default is NULL and not output to file
#' @param fusions_color In one group, different fusions show different colors, 
#' default is NULL and use the setted theme color
#' @param ... Parameters pass to \code{\link{set_colors}}
#' 
#' @return 
#' A data frame
#' @export
#' @examples 
#' 
#' meta_template <- system.file('extdata', 
#' 'demo/proteinpaint/heatmap_meta_template.txt', package = 'ngstk')
#' raw_meta <- read.table(meta_template, sep = '\t', header = TRUE)
#' meta_test_1 <- raw_meta
#' term <- group <- 'Fusions'
#' meta_test_1$term <- term
#' meta_test_1$group <- group
#' meta_test_1$value <- c(rep(c('ZNF384-Fusions', 'MEF2D-Fusions'), 
#' 3), 'TCF3-PBX1')
#' meta_fusions <- format_pp_meta_fusions(meta_test_1)
#' meta_test_2 <- raw_meta
#' term <- group <- c(rep(c('MEF2D-Fusions', 'ZNF384-Fusions'), 
#' 3), 'DUX4-Fusions')
#' meta_test_2$term <- term
#' meta_test_2$group <- group
#' meta_test_2$value <- c('MEF2D-PA', 'EP300-ZNF384', 
#' 'MEF2D-PB', 'ABC-ZNF384', 'MEF2D-PB', 'ABD-ZNF384', 
#'                        'DUX4-IGH')
#' meta_fusions <- format_pp_meta_fusions(meta_test_2)
format_pp_meta_fusions <- function(raw_meta, outfn = NULL, fusions_color = NULL, 
  ...) {
  if (is.null(fusions_color)) {
    fusions_color <- set_colors(...)
  }
  pp_meta <- raw_meta
  pp_meta$color <- ""
  pp_meta$value <- str_replace(pp_meta$value, " ", "")
  pp_meta$legendorder[is.na(pp_meta$legendorder)] <- ""
  term_unique <- unique(pp_meta$term)
  for (i in term_unique) {
    value_unique <- unique(pp_meta[pp_meta$term == i, "value"])
    for (j in 1:length(value_unique)) {
      index <- which(pp_meta$value == value_unique[j])[1]
      pp_meta$color[index] <- fusions_color[j]
    }
  }
  pp_meta$value <- str_replace(pp_meta$value, "#N/A", "NA")
  pp_meta <- pp_meta[pp_meta$value != "NA", ]
  if (!is.null(outfn)) {
    write.table(pp_meta, outfn, sep = "\t", row.names = F, quote = F)
  }
  return(pp_meta)
}



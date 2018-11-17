#' Functions to enable our OptionParser to recognize specific command line options (optparse).
#'
#' @param ... Parameters pass to \code{\link[optparse]{make_option}}
#' @export
#' @examples
#' # example from vignette
#' option_list <- list(
#'   make_option(c('-v', '--verbose'), action='store_true', default=TRUE,
#'       help='Print extra output [default]'),
#'   make_option(c('-q', '--quietly'), action='store_false',
#'       dest='verbose', help='Print little output'),
#'   make_option(c('-c', '--count'), type='integer', default=5,
#'       help='Number of random normals to generate [default %default]',
#'       metavar='number'),
#'   make_option('--generator', default='rnorm',
#'       help = 'Function to generate random deviates [default \'%default\']'),
#'   make_option('--mean', default=0,
#'       help='Mean if generator == \'rnorm\' [default %default]'),
#'   make_option('--sd', default=1, metavar='standard deviation',
#'       help='Standard deviation if generator == \'rnorm\' [default %default]')
#'  )
make_option <- function(...) {
  optparse::make_option(...)
}

#' Parse command line options (optparse).
#' 
#' @param ... Parameters pass to \code{\link[optparse]{parse_args}}
#' @export
#' @examples
#' 
#' # example from vignette
#' option_list <- list(
#'   make_option(c('-v', '--verbose'), action='store_true', default=TRUE,
#'       help='Print extra output [default]'),
#'   make_option(c('-q', '--quietly'), action='store_false',
#'       dest='verbose', help='Print little output'),
#'   make_option(c('-c', '--count'), type='integer', default=5,
#'       help='Number of random normals to generate [default %default]',
#'       metavar='number'),
#'   make_option('--generator', default='rnorm',
#'       help = 'Function to generate random deviates [default \'%default\']'),
#'   make_option('--mean', default=0,
#'       help='Mean if generator == \'rnorm\' [default %default]'),
#'   make_option('--sd', default=1, metavar='standard deviation',
#'       help='Standard deviation if generator == \'rnorm\' [default %default]')
#'   )
#' parse_args(opt_parser(option_list = option_list), args = c('--sd=3', '--quietly'))
#'
parse_args <- function(...) {
  optparse::parse_args(...)
}

#' Parse command line options.
#' 
#' @param ... Parameters pass to \code{\link[optparse]{parse_args2}}
#' @export
#' @examples
#' #example from vignette using positional arguments
#' option_list2 <- list(
#'   make_option(c('-n', '--add-numbers'), action='store_true', default=FALSE,
#'       help='Print line number at the beginning of each line [default]')
#'   )
#' parser <- opt_parser(usage = '%prog [options] file', option_list=option_list2)
#'
#' parse_args(parser, args = c('--add-numbers', 'example.txt'), positional_arguments = TRUE)
#'
#' parse_args(parser, args = c('--add-numbers', 'example.txt'), positional_arguments = TRUE,
#'         convert_hyphens_to_underscores = TRUE)
#' parse_args2(parser, args = c('--add-numbers', 'example.txt'))
parse_args2 <- function(...) {
  optparse::parse_args2(...)
}

#' A function to create an instance of a parser object with subcommands.
#'
#' Modifed from \code{\link[optparse]{OptionParser}}
#' @param subcmds_list A list setting the subcommands name and short description 
#' (e.g. list(subcmd1='Method1 to plot boxplot'))
#' @param subcmds_sections A list setting the sections of subcommands 
#' (e.g. list(subcmd_group_1=c('subcmd1', 'subcmd2'), subcmd_group_2=c('subcmd2')))
#' @param ... Parameters pass to \code{\link[optparse]{OptionParser}}
#' @export
#' @examples
#'
#' option_list <- list(
#'   make_option(c('-l', '--list-all-subcmds'), action = 'store_true', 
#'               default = FALSE, help = 'Print all supported subcmds of ngsjs.')
#' )
#' subcmds_list <- list(subcmd1 = 'Use method 1 to plot boxplot', 
#'                      subcmd2 = 'Use method 2 to plot boxplot')
#' description <- 'Method to plot boxplot'
#' usage <- 'usage: %prog [options] [params]'
#' opt_parser_obj <- opt_parser(subcmds_list = subcmds_list, 
#'                             option_list = option_list,
#'                             description = description,
#'                             usage = usage)
opt_parser <- function(subcmds_list = list(), subcmds_sections = list(), ...) {
  description_prefix = "Description:\n"
  subcmds_start_flag = "@subcmds@"
  if (length(subcmds_sections) == 0 && length(subcmds_list) != 0) {
    subcmds_sections <- list(Commands = names(subcmds_list))
  }
  params <- list(...)
  if (length(subcmds_list) != 0) {
    subcmds_desc <- subcmds_start_flag
    for (subcmd_section in names(subcmds_sections)) {
      subcmds <- subcmds_sections[[subcmd_section]]
      desc_tmp <- paste0(sprintf("\t%-12s", subcmds), "  ", subcmds_list[subcmds], 
        collapse = "\n")
      subcmds_desc <- sprintf("%s\n%s:\n%s", subcmds_desc, subcmd_section, 
        desc_tmp)
    }
    if ("description" %in% names(params)) {
      params$description <- sprintf("%s%s%s", description_prefix, params$description, 
        subcmds_desc)
    } else {
      params$description <- sprintf("%s", subcmds_desc)
    }
    opt_parser_obj <- do.call(OptionParser, params)
  } else if ("description" %in% names(params)) {
    params$description <- sprintf("%s%s", description_prefix, params$description)
    opt_parser_obj <- do.call(OptionParser, params)
  } else {
    opt_parser_obj <- do.call(OptionParser, params)
  }
  return(opt_parser_obj)
}

# Copied from https://github.com/trevorld/optparse/blob/master/R/optparse.R make
# print_help work
.as_string <- function(default) {
  if (is.null(default)) {
    default_str <- "NULL"
  } else if (!length(default)) {
    default_str <- paste0(typeof(default), "(0)")
  } else if (is.na(default)) {
    default_str <- "NA"
  } else {
    default_str <- as.character(default)
  }
}

#' Printing an usage message from an OptionParser object 
#'
#' Modifed from \code{\link[optparse]{print_help}}
#' @param object A opt_parser instance.
#' @param help_order The order to print the help message ['description', 'usage', 'options', 'subcmds', 'epilogue']
#' @export
#' @examples
#' option_list <- list(
#'   make_option(c('-l', '--list-all-subcmds'), action = 'store_true', 
#'   default = FALSE, help = 'Print all supported subcmds of ngsjs.')
#' )
#' subcmds_list <- list(subcmd1 = 'Use method 1 to plot boxplot', 
#'                      subcmd2 = 'Use method 2 to plot boxplot')
#' description <- 'Method to plot boxplot'
#' usage <- 'usage: %prog [options] [params]'
#' opt_parser_obj <- opt_parser(subcmds_list = subcmds_list, 
#'                             option_list = option_list,
#'                             description = description,
#'                             usage = usage)
#' print_help(opt_parser_obj)
print_help <- function(object, help_order = c("description", "usage", "options", 
  "subcmds", "epilogue")) {
  subcmds_start_flag = "@subcmds@"
  if (length(grep(subcmds_start_flag, object@description)) > 0) {
    desc_split <- strsplit(object@description, subcmds_start_flag)
    object@description <- desc_split[[1]][1]
    subcmds <- desc_split[[1]][2]
  } else {
    subcmds <- ""
  }
  for (i in help_order) {
    if (i == "usage") {
      cat(object@usage, fill = TRUE)
      cat("\n")
    }
    if (i == "description") {
      cat(object@description, fill = TRUE)
      cat("\n")
    }
    if (i == "subcmds") {
      cat(subcmds, fill = TRUE)
    }
    if (i == "options") {
      cat("Options:", sep = "\n")
      options_list <- object@options
      for (ii in seq_along(options_list)) {
        option <- options_list[[ii]]
        cat("\t")
        if (!is.na(option@short_flag)) {
          cat(option@short_flag)
          if (option@action == "store") {
          cat(" ", toupper(option@metavar), sep = "")
          }
          cat(", ")
        }
        if (!is.null(option@long_flag)) {
          cat(option@long_flag)
          if (option@action == "store") {
          cat("=", toupper(option@metavar), sep = "")
          }
        }
        cat(sprintf("\n\t%-14s", ""))
        cat(sub("%default", .as_string(option@default), option@help))
        cat("\n")
      }
    }
    if (i == "epilogue") 
      cat(object@epilogue, fill = TRUE)
  }
  return(invisible(NULL))
}

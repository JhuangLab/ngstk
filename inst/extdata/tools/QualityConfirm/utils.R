# Orther function

# Functions used in initialize
# -----------------------------------------
# Set default args
SetDefault <- function(inarg, default) {
  if (class(inarg) == "NULL") {
    return(default)
  } else if (class(inarg) == "character" && inarg == '') {
    return(default)
  } else if (class(inarg) == "integer" && (length(inarg) == 0 || is.na(inarg))) {
    return(default)
  } else {
    return(inarg)
  }
  # if (is.null(inarg)) {
  # return(default)
  # } else if (inarg == '') {
  #   return(default)
  # } else if (is.na(inarg)) {
  #   return(default)
  # } else {
  #   return(inarg)
  # }
}

# Make dir which not exist
MakeDir <- function(checkDir) {
  if (!dir.exists(checkDir)) {
    dir.create(checkDir)
  }
}


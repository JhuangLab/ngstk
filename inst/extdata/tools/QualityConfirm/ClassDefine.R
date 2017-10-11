source(file = "./utils.R")
source(file = "./ClassMethod.R")

setClass("ReadsCheck", slots = list(samtools = "character", refhg = "character", maxDepth = "numeric", bedGap = "numeric", 
  panelBed = "character", bamDir = "character", bamList = "character", resultDir = "character", depthDir = "character", 
  figDir = "character", sumDir = "character", mergeBedPath = "character", currBamNum = "numeric", status = "numeric", 
  mergedBed = "data.frame", logDir = "character", proMessage = "character"))

# argument list: argList <- list( samtools = 'path to samtools', refhg = 'path to human genome
# reference', panelBed = 'path to bed file', bamDir = 'path to bam directory' bamList = 'the bam files
# to test', // If not set bam list, all bam files in the dir will be used.  resultDir = 'path to result'
# maxDepth = 40000 bedGap = 2 )

setMethod("initialize", "ReadsCheck", function(.Object, argList) {
  # env
  samtools <- unname(Sys.which("samtools"))
  if (samtools == "") {
    samtools <- Sys.getenv("SAMTOOLS")
  }
  if (samtools == "") {
    stop("Please set environment variable SAMTOOLS, path of samtools executable file")
  }
  reffa <- Sys.getenv("REFFA")
  if (reffa == "") {
    stop("Please set environment variable REFFA, path of reference genome")
  }
  .Object@samtools <- SetDefault(argList$samtools, samtools)
  .Object@refhg <- SetDefault(argList$refhg, reffa)
  
  # input files or dirs
  .Object@panelBed <- SetDefault(argList$panelBed, list.files(pattern = "*.bed"))
  .Object@bamDir <- SetDefault(argList$bamDir, ".")
  .Object@bamList <- SetDefault(argList$bamList, list.files(path = .Object@bamDir, pattern = "*.bam"))
  if (length(.Object@bamList) == 0) {
    print("You must define a directory which has bam files!")
  }
  if (length(.Object@panelBed) == 0) {
    print("You must define a panel bed file!")
  }
  
  # input parameters
  .Object@maxDepth <- SetDefault(as.integer(argList$maxDepth), 40000)
  .Object@bedGap <- SetDefault(as.integer(argList$bedGap), 2)
  
  # result output
  .Object@resultDir <- SetDefault(argList$resultDir, "./result")
  .Object@depthDir <- paste0(.Object@resultDir, "/depth")
  .Object@figDir <- paste0(.Object@resultDir, "/fig")
  .Object@sumDir <- paste0(.Object@resultDir, "/summary")
  .Object@mergeBedPath <- paste0(.Object@resultDir, "/merge.panel.bed")
  
  
  # log dir and files
  .Object@logDir <- SetDefault(argList$logDir, "/log")
  
  # process initial
  .Object@currBamNum <- 1
  .Object@status <- 0
  
  # make result dir
  MakeDir(.Object@resultDir)
  MakeDir(.Object@depthDir)
  MakeDir(.Object@figDir)
  MakeDir(.Object@sumDir)
  
  return(.Object)
  
})

options(stringsAsFactors = FALSE)
source("https://bioconductor.org/biocLite.R")
library(ngstk)
required_packages <- c("annovarR", "stringr", "BiocParallel", "DEXSeq", "DESeq2")
for (i in required_packages) {
  status <- supress_any_message(require(i, quietly = TRUE, character.only = TRUE))
  if (!status) {
    biocLite(i)
  }
  supress_any_message(require(i, quietly = TRUE, character.only = TRUE))
}
BPPARAM = MulticoreParam(workers = 2)

workdir <- "./"
setwd(workdir)
data.dir <- "./htseq_count/"

# groups is a data.frame, 1st column is '', 2nd column is ''
groups <- read.csv("group.txt", sep = "\t")

deseq2_diff <- function(fout, condtion1, condtion2) {
  sampleName <- c(condtion1, condtion2)
  countFiles <- paste(sampleName, ".txt", sep = "")
  sampleCondition <- c(rep("all", length(sampleName)))
  names(sampleCondition) <- sampleName
  sampleCondition[condtion1] <- "con1"
  sampleTable <- data.frame(sampleName = sampleName, fileName = countFiles, condition = as.factor(sampleCondition))  #as.factor by panda2017.05.07
  ddsHTSeq <- DESeqDataSetFromHTSeqCount(sampleTable = sampleTable, directory = data.dir, 
    design = ~condition)
  
  # fil <- rowSums(counts(ddsHTSeq)>10) >= 5
  fil <- rowSums(counts(ddsHTSeq) > 10) >= 1
  ddsHTSeq1 <- ddsHTSeq[fil, ]
  # co.var <- function(x) ( var(x) ) dat.var <- apply(counts(ddsHTSeq1),1,co.var)
  # index <- dat.var >= quantile(dat.var, probs = 0.4) ddsHTSeq2 <-
  # ddsHTSeq1[index,]
  ddsHTSeq2 <- ddsHTSeq1
  
  dds <- DESeq(ddsHTSeq2, BPPARAM = BPPARAM, parallel = TRUE)
  # dds<-DESeq(ddsHTSeq2)
  
  res <- data.frame(results(dds))
  print(res)
  res <- res[order(res$padj), ]
  ens2gene <- annovarR::annotation(dat = row.names(res), anno.name = "bioc_org_hs_eg", keytype = "ENSEMBL", 
    columns = "SYMBOL")
  ens2refseq <- annovarR::annotation(dat = row.names(res), anno.name = "bioc_org_hs_eg", keytype = "ENSEMBL", 
    columns = "REFSEQ")
  print(ens2gene)
  print(ens2refseq)
  print(row.names(res))
  i <- match(row.names(res), ens2gene[, ENSEMBL])
  res$symbol <- ens2gene[i, SYMBOL]
  i <- match(row.names(res), ens2refseq[, ENSEMBL])
  res$refseq <- ens2refseq[i, REFSEQ]
  write.table(res, fout, sep = "\t")
}
myfout <- "diffexp.txt"
#subgroup <- groups[groups[, 2] == "g9", ]
subgroup <- groups
G1 <- subgroup[subgroup[, 3] == "1", 1]
G2 <- subgroup[subgroup[, 3] == "0", 1]
deseq2_diff(fout = myfout, condtion1 = G1, condtion2 = G2)


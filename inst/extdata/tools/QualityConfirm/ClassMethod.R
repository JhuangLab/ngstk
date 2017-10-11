library(RColorBrewer)
library(ggplot2)
library(stringr)

# function for check class
ClassCheck <- function(Obj) {
  if (class(Obj) != "ReadsCheck") {
    stop("The input is not a 'ReadsCheck' object!")
  }
}

# Function to get depth of each base from bam.
# Once a bam file. 
ReadsDepth <- function(Obj, test = FALSE) {
  ClassCheck(Obj)
  tag <- Obj@bamList[Obj@currBamNum]
  currentBam <- paste0(Obj@bamDir, '/', tag)
  depthFile <- paste0(Obj@depthDir, '/', tag, '.depth')
  panelBed <- Obj@panelBed[1]
  if (! (file.exists(currentBam) && file.exists(panelBed))) {
    # add log ---------------------------------
    stop('The bam file and/or panel bed file not exist!')
  }
  
  cmd <- sprintf("%s depth -d %s -b %s %s > %s", 
                 Obj@samtools, Obj@maxDepth, panelBed, currentBam, depthFile)
  # add log ---------------------------------
  print(sprintf("Execute: %s", cmd))
  if (! test) {
    cmdResult <- system(cmd, intern = TRUE)
  }
  Obj@status <- Obj@status + 1
  return(Obj)
}

# Function to merge panel bed.
PanelMerge <- function(Obj) {
  ClassCheck(Obj)
  options(stringsAsFactors = FALSE)
  panelFile <- Obj@panelBed[1]
  if (! file.exists(panelFile) ) {
    # add log ------------------------------------------
    stop('The panel bed file not exist!')
  }
  
  panel_raw <- read.csv(panelFile, sep = '\t', header = T)
  gap <- Obj@bedGap
  for(i in 1:nrow(panel_raw)) {
    if (i == 1) {
      panel_new <- panel_raw[1,]
      i_new = 1
    } else{
      temp <- panel_raw[i,]
      if (temp$gene != panel_raw$gene[i-1]){
        panel_new <- rbind(panel_new, temp)
        i_new = i_new + 1 
      } else{
        comp_end <- panel_new$end[i_new]
        comp_start <- temp$start
        if (comp_end+gap >= comp_start){
          panel_new$end[i_new] <- temp$end
        } else {
          panel_new <- rbind(panel_new, temp)
          i_new = i_new + 1
        }
      }
    }
  }
  panel_new <- cbind(panel_new, part=c(0))
  for (i in 1:nrow(panel_new)) {
    if (i == 1){
      panel_new$part =1
      counts = 1
    } else {
      if (panel_new$gene[i] == panel_new$gene[i-1]) {
        counts = counts+1
        panel_new$part[i] = counts
      } else {
        counts = 1
        panel_new$part[i] = counts
      }
    }
  }
  
  chrlist <- c('chr1','chr2', 'chr3', 'chr4','chr5','chr6','chr7', 'chr8', 'chr9',
               'chr10','chr11','chr12','chr13', 'chr14', 'chr15', 'chr16', 'chr17',
               'chr18', 'chr19', 'chr20','chr21', 'chrX', 'chrY')
  panel_new$chr <- factor(panel_new$chr, levels = chrlist)
  panel_new <- panel_new[order(panel_new$chr, panel_new$start), ]
  panel_new <- cbind(panel_new, x_start=rep(0, nrow(panel_new)), x_end=rep(0, nrow(panel_new)))
  for (rownum in 1:nrow(panel_new)) {
    if ( rownum == 1) {
      panel_new$x_start[rownum] <- 1
      panel_new$x_end[rownum] <- panel_new$x_start[rownum] + (panel_new$end[rownum]-panel_new$start[rownum])
    } else {
      panel_new$x_start[rownum] <- panel_new$x_end[rownum-1] + 2
      panel_new$x_end[rownum] <- panel_new$x_start[rownum] + (panel_new$end[rownum]-panel_new$start[rownum])
    }
  }
  
  write.table(panel_new, file = Obj@mergeBedPath, quote = FALSE, sep = '\t', 
              row.names = FALSE, col.names = T)
  Obj@mergedBed <- panel_new
  return(Obj)
}

# Function to plot base covering.
# Once a depth file.
PlotCountFig <- function(Obj) {
  options(stringsAsFactors = F)
  ClassCheck(Obj)
  # -------------------------------------------------------------------
  chrlist <- c('chr1','chr2', 'chr3', 'chr4','chr5','chr6','chr7', 'chr8', 'chr9',
               'chr10','chr11','chr12','chr13', 'chr14', 'chr15', 'chr16', 'chr17',
               'chr18', 'chr19', 'chr20','chr21', 'chrX', 'chrY')
  #--------------------------------------------------------------------
  if (nrow(Obj@mergedBed) == 0) {
    if (file.exists(Obj@mergeBedPath)) {
      Obj@mergedBed <- read.table(Obj@mergeBedPath, sep = '\t', header = T)
    } else {
      Obj <- PanelMerge(Obj)
    }
  }
  mergedBed <- Obj@mergedBed

  
  tag <- Obj@bamList[Obj@currBamNum]
  depthFile <- paste0(Obj@depthDir, '/', tag, '.depth')
  figPrefix <- paste0(Obj@figDir, '/', tag, '.countFig')
  
  # input depth data
  if (! file.exists(depthFile)) {
    stop("Must use 'ReadsDepth' function before!")
  }
  depthData <- read.csv(depthFile, sep='\t', header = F)
  depthData <- cbind(depthData, index=rep(1:nrow(depthData)))
  colnames(depthData) <- c('chr', 'pos', 'reads', 'index')
  depthData$chr <- factor(depthData$chr, levels = chrlist)
  depthData <- depthData[order(depthData$chr, depthData$pos), ]
  depthData <- cbind(depthData, x=c(0), gene=c('no'), part=c(0))
  
  # annotate panel info: gene, x site, and part to depth data
  i_depth = 1
  i_panel = 1
  while (i_depth <= nrow(depthData)) {
    if (depthData$reads[i_depth] == 0) { depthData$reads[i_depth] = 1}
    if (depthData$chr[i_depth] == mergedBed$chr[i_panel] & 
        depthData$pos[i_depth] >= mergedBed$start[i_panel] & 
        depthData$pos[i_depth] <= mergedBed$end[i_panel]) {
      depthData$gene[i_depth] = mergedBed$gene[i_panel]
      depthData$part[i_depth] = mergedBed$part[i_panel]
      depthData$x[i_depth] =  mergedBed$x_start[i_panel] + (depthData$pos[i_depth]-mergedBed$start[i_panel])
      i_depth = i_depth+1
    } else {
      i_panel = i_panel+1
    }
  }
  
  # Generate figure for each gene
  genelist <- mergedBed$gene
  genelist <- unique(genelist)
  color_group <- c(brewer.pal(12, 'Set3'), brewer.pal(9, 'Set1'),
                   brewer.pal(8, 'Dark2')[1:4])
  
  for (aim_gene in genelist) {
    sub_mergedBed <- subset(mergedBed, gene==aim_gene)
    start_xsite <- min(sub_mergedBed$x_start)
    end_xsite <- max(sub_mergedBed$x_end)
    plotdata <- data.frame(chr=sub_mergedBed$chr[1], pos=c(0), reads=c(1), 
                           x=seq(start_xsite, end_xsite), gene=c(aim_gene), part=c(0))
    i_plot = 1
    i_panel = 1
    while (i_plot <= nrow(plotdata)) {
      if (plotdata$x[i_plot] >= sub_mergedBed$x_start[i_panel] & 
          plotdata$x[i_plot] <= sub_mergedBed$x_end[i_panel]) {
        
        plotdata$part[i_plot] = sub_mergedBed$part[i_panel]
        plotdata$pos[i_plot] =  sub_mergedBed$start[i_panel] + (plotdata$x[i_plot]-sub_mergedBed$x_start[i_panel])
        readdata <- subset(depthData, subset=(gene==aim_gene & pos==plotdata$pos[i_plot]))
        if (length(readdata$reads) > 0) {
          plotdata$reads[i_plot] <- readdata$reads
        }
        if (i_plot == 1 & plotdata$reads[i_plot] == 1) {
          plotdata <- plotdata[-i_plot,]
          next
        } else if (i_plot > 1) {
          if (plotdata$part[i_plot] != plotdata$part[i_plot-1] & 
              plotdata$reads[i_plot] == 1) {
            plotdata <- plotdata[-i_plot,]
            next
          }
        }
        i_plot = i_plot+1
      } else {
        plotdata <- plotdata[-i_plot,]
        i_panel = i_panel+1
      }
    }
    plotdata$part <- factor(plotdata$part)
    
    
    # Get annotatation position text and legend text
    legend_labels <- c()
    part_tag <- c()
    for (i in 1:nrow(sub_mergedBed)) {
      temp <- paste0(sub_mergedBed$chr[i], ':', sub_mergedBed$start[i], '-', sub_mergedBed$end[i])
      part_tag <- c(part_tag, temp)
      temp <- paste0(sub_mergedBed$chr[i], ':', sub_mergedBed$start[i], '-', sub_mergedBed$end[i])
      legend_labels <- c(legend_labels, temp)
    }
    
    ggplot(plotdata, aes(x=x, y=log10(reads))) +
      geom_line(aes(color=part), position = 'identity', size=0.5) +
      annotate('rect', xmin=sub_mergedBed$x_start, xmax=sub_mergedBed$x_end, ymin=0, ymax=5,
               alpha=.1, fill=color_group[1:nrow(sub_mergedBed)]) +
      # annotate('text', x=(subgene_xis$start_x+subgene_xis$end_x)/2, y=0.5, 
      #         angle=45, label = part_tag, vjust=0.3) +
      xlim(start_xsite, end_xsite) + ylim(3, 5) +
      # scale_x_continuous(breaks = x_scale_breaks, labels = x_scale_labels, 
      #                   name = paste0('Position: ',plotdata$chr[1],'\n',aim_gene) ) +
      # scale_x_continuous(name=aim_gene, breaks=x_breaks ) +
      scale_x_continuous(name=aim_gene, breaks=(sub_mergedBed$x_start+sub_mergedBed$x_end)/2,
                         labels=part_tag) +
      scale_y_continuous(name="Log10( reads number )") + 
      scale_color_discrete(name='Regions', labels=legend_labels ) +
      # ggtitle(substr(depthDatafile, 1, regexpr('\\.all.', depthDatafile)[1]-1)) +
      theme(
        axis.text.x = element_text(angle = 90, hjust=0,  vjust=0, size = 12, colour = 'black', face = 'plain'),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, colour = 'black')
      )
    
    figfile <- paste(figPrefix, aim_gene, 'pdf', sep = '.')
    figwidth=(end_xsite - start_xsite) %/% 100 +4
    if (figwidth < 25) { figwidth <- 25+4}
    ggsave(figfile, width = figwidth, height = 20, units = 'cm', limitsize = FALSE)
  }
  
  Obj@status <- Obj@status + 2
  return(Obj)
}

# Function to get reads summary.
# Onece a bam.
SingleReadsSummary <- function(Obj) {
  options(stringsAsFactors = F)
  ClassCheck(Obj)
  # -------------------------------------------------------------------
  chrlist <- c('chr1','chr2', 'chr3', 'chr4','chr5','chr6','chr7', 'chr8', 'chr9',
               'chr10','chr11','chr12','chr13', 'chr14', 'chr15', 'chr16', 'chr17',
               'chr18', 'chr19', 'chr20','chr21', 'chrX', 'chrY')
  #--------------------------------------------------------------------
  # Get merged panel.
  if (nrow(Obj@mergedBed) == 0) {
    if (file.exists(Obj@mergeBedPath)) {
      Obj@mergedBed <- read.table(Obj@mergeBedPath, sep = '\t', header = T)
    } else {
      Obj <- PanelMerge(Obj)
    }
  }
  mergedBed <- Obj@mergedBed
  
  tag <- Obj@bamList[Obj@currBamNum]
  depthFile <- paste0(Obj@depthDir, '/', tag, '.depth')
  sumFile <- paste0(Obj@sumDir, '/', tag, '.summary.csv')

  # input depth data
  if (! file.exists(depthFile)) {
    stop("Must use 'ReadsDepth' function before!")
  }
  depthData <- read.csv(depthFile, sep='\t', header = F)
  colnames(depthData) <- c('chr', 'pos', 'reads')
  depthData$chr <- factor(depthData$chr, levels = chrlist)
  depthData <- depthData[order(depthData$chr, depthData$pos), ]

  # annotate panel info to APL data frame
  sumtable <- data.frame()
  
  for (i in 1:nrow(mergedBed)) {
    gene <- mergedBed$gene[i]
    chr <- as.character(mergedBed$chr[i])
    pos_start <- mergedBed$start[i]
    pos_end <- mergedBed$end[i]
    temp <- subset(depthData, 
                   subset = (chr == chr & pos >= pos_start & pos <= pos_end ))
    
    ## summury
    len <- pos_end - pos_start + 1
    coverage <- nrow(subset(temp, subset = (reads > 0)))
    coverage.ratio <- coverage / len
    max.reads <- max(temp$reads)
    mean.reads <- sum(temp$reads) / len
    low.reads.num <- nrow(subset(temp, subset = (reads <= 1000 & reads > 0)))
    high.reads.num <- nrow(subset(temp, subset = (reads > 1000)))
    if (coverage > 0) {
      low.reads.cover <- low.reads.num / coverage
      high.reads.cover <- high.reads.num / coverage
    } else {
      low.reads.cover <- 0
      high.reads.cover <- 0
    }
    result <- list(sample=tag, gene=gene, chr=chr, start=pos_start, end=pos_end, length=len,
                   coverage=coverage, coverage.ratio=coverage.ratio, max.reads=max.reads, 
                   mean.reads=mean.reads, low.reads.num.1000=low.reads.num, low.reads.cover=low.reads.cover, 
                   high.reads.num.1000=high.reads.num, high.reads.cover=high.reads.cover)
    sumtable <- rbind(sumtable, result)
    # change -Inf to 0
    inf.row <- sumtable[, 'max.reads'] == (-Inf)
    sumtable[inf.row, 'max.reads'] <- 0
  }
  
  write.csv(sumtable, sumFile)
  
  Obj@status <- Obj@status + 4
  Obj@currBamNum <- Obj@currBamNum + 1
  return(Obj)
}

# Function to plot panel histogram
PlotBedHist <- function(Obj) {
  ClassCheck(Obj)
  options(stringsAsFactors = FALSE)
  panelFile <- Obj@panelBed[1]
  if (! file.exists(panelFile) ) {
    # add log ------------------------------------------
    stop('The panel bed file not exist!')
  }
  
  # Plot histogram of original panel .
  panel.data <- read.csv(panelFile, sep = '\t', header = T)
  panel.data <- cbind(panel.data, len=c(0), part=c(0), index=c(1:nrow(panel.data)))
  for (i in 1:nrow(panel.data)) {
    if (i == 1) {
      part.up <- panel.data$gene[i]
      part.count <- 0
    }
    part.now <- panel.data$gene[i]
    panel.data$len[i] <- panel.data$end[i] - panel.data$start[i] + 1
    if (part.now == part.up) {
      part.count <- part.count + 1
      panel.data$part[i] <- part.count
      
    } else {
      part.count <- 1
      part.up <- part.now
      panel.data$part[i] <- part.count
    }
  }
  panelFig <- paste0(Obj@sumDir, '/orig.panel.hist.pdf')
  pdf(panelFig)
  x <- panel.data$len
  h <- hist(x, breaks=30, col="red", xlab="Length of regions", 
            main="Histogram of panel regions") 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  dev.off()
  
  # Plot histogram of merged panel
  if (nrow(Obj@mergedBed) == 0) {
    if (file.exists(Obj@mergeBedPath)) {
      Obj@mergedBed <- read.table(Obj@mergeBedPath, sep = '\t', header = T)
    } else {
      Obj <- PanelMerge(Obj)
    }
  }
  panel.data <- Obj@mergedBed
  panel.data <- cbind(panel.data, len=c(0), part=c(0), index=c(1:nrow(panel.data)))
  for (i in 1:nrow(panel.data)) {
    if (i == 1) {
      part.up <- panel.data$gene[i]
      part.count <- 0
    }
    part.now <- panel.data$gene[i]
    panel.data$len[i] <- panel.data$end[i] - panel.data$start[i] + 1
    if (part.now == part.up) {
      part.count <- part.count + 1
      panel.data$part[i] <- part.count
      
    } else {
      part.count <- 1
      part.up <- part.now
      panel.data$part[i] <- part.count
    }
  }
  mergePanelFig <- paste0(Obj@sumDir, '/merged.panel.hist.pdf')
  pdf(mergePanelFig)
  x <- panel.data$len
  h <- hist(x, breaks=30, col="red", xlab="Length of regions", 
            main="Histogram of merged panel regions") 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  dev.off()
  
  return(Obj)
}

# Function combine all single sample summary files and analysis.
AllReadsSummary <- function(Obj) {
  options(stringsAsFactors = F)
  # Panel data
  if (nrow(Obj@mergedBed) == 0) {
    if (file.exists(Obj@mergeBedPath)) {
      Obj@mergedBed <- read.table(Obj@mergeBedPath, sep = '\t', header = T)
    } else {
      Obj <- PanelMerge(Obj)
    }
  }
  mergedBed <- Obj@mergedBed
  # Read all summary files
  sumFiles <- list.files(path = Obj@sumDir, pattern = 'summary.csv')
  data <- data.frame()
  for (i in sumFiles) {
    csvFile <- paste0(Obj@sumDir, '/', i)
    temp <- read.csv(csvFile)
    data <- rbind(data, temp)
  }
  data <- data[, -1]
  # Summary data analysis
  samplelist <- unique(data$sample)
  genelist <- unique(data$gene)
  table.data.in <- data[, c('sample', 'gene', 'length', 'coverage', 'mean.reads', 'low.reads.num.1000', 'high.reads.num.1000')]
  table.data <- data.frame()
  for (i in samplelist) {
    for (j in genelist) {
      temp <- subset(table.data.in, subset = (sample == i & gene == j))
      all.length <- sum(temp$length)
      all.coverage <- sum(temp$coverage)
      all.coverage.ratio <- all.coverage / all.length
      all.high.num <- sum(temp$high.reads.num)
      all.high.ratio <- all.high.num / all.length
      all.mean.reads <- sum(temp$length * temp$mean.reads) / all.length
      line.list <- list(sample=i, gene=j, length=all.length, coverage=all.coverage, coverage.ratio=all.coverage.ratio,
                        mean.reads=all.mean.reads, high.reads.num=all.high.num, high.reads.cover=all.high.ratio)
      table.data <- rbind(table.data, line.list)
    }
  }
  write.csv(table.data, paste0(Obj@sumDir, '/analy.all.gene.csv'))
  
  data <- merge(data, mergedBed, by=c('chr', 'start', 'end', 'gene'))
  data <- data[order(data$sample), ]
  #low.read.ratio <-data$low.reads.num / data$length
  #data <- cbind(data, low.read.ratio=low.read.ratio)
  
  zero.region <- subset(data, subset = (coverage == 0))
  zero.region <- zero.region[, c('sample', 'gene', 'chr', 'start', 'end', 'part', 'length', 'coverage')]
  zero.gene <- unique(zero.region$gene)
  zero.reads.data <- data.frame()
  for (i in zero.gene) {
    temp <- subset(zero.region, subset = (gene == i)) 
    part.list <- unique(temp$part)
    for (j in part.list) {
      sub.temp <- subset(temp, subset = (part == j))
      zero.count <- nrow(sub.temp)
      line.list <- list(gene=sub.temp$gene[1], chr=sub.temp$chr[1], start=sub.temp$start[1], end=sub.temp$end[1],
                        length=sub.temp$length[1], coverage=sub.temp$coverage[1], sample.count=zero.count)
      zero.reads.data <- rbind(zero.reads.data, line.list)
    }
  }
  
  write.csv(zero.reads.data, paste0(Obj@sumDir, '/analy.nonreads,region.csv'))
  
  low.reads.region <- subset(data, subset = (low.reads.cover == 1))
  low.reads.region <- low.reads.region[, c('gene', 'chr', 'start', 'end', 'part', 'length','mean.reads', 
                                           'low.reads.cover')]
  low.gene <- unique(low.reads.region$gene)
  low.reads.data <- data.frame()
  for (i in low.gene) {
    temp <- subset(low.reads.region, subset = (gene == i))
    part.list <- unique(temp$part)
    for (j in part.list) {
      sub.temp <- subset(temp, subset = (part == j))
      part.mean <- sum(sub.temp$mean.reads) / nrow(sub.temp)
      line.list <- list(gene=sub.temp$gene[1], chr=sub.temp$chr[1], start=sub.temp$start[1], end=sub.temp$end[1],
                        length=sub.temp$length[1], mean.reads=part.mean)
      low.reads.data <- rbind(low.reads.data, line.list)
    }
  }
  low.reads.data <- unique(low.reads.data)
  write.csv(low.reads.data, paste0(Obj@sumDir, '/analy.lowreads.region.csv'))
  
  meanConverDat <- c(0)
  for (i in samplelist) {
    temp <- subset(data, subset = (sample == i))
    temp <- temp[order(temp$chr, temp$start), ]
    temp <- temp$mean.reads
    meanConverDat <- meanConverDat + temp
  }
  meanConverDat <- meanConverDat / length(samplelist)
  
  pdf(paste0(Obj@sumDir, '/mean.coverage.hist.pdf'))
  x <- meanConverDat
  h <- hist(x, breaks=30, col="red", xlab="Mean coverage", 
            main="Histogram of mean region coverage") 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  dev.off()
  
  return(Obj)
}

# test
# Obj <- new("ReadsCheck", list(panelBed = './example/APL_panel.bed', bamDir = './example'))
# Obj <- ReadsDepth(Obj, test = T)
# Obj <- PanelMerge(Obj)
# Obj <- PlotCountFig(Obj)
# Obj <- PlotBedHist(Obj)
# Obj <- SingleReadsSummary(Obj)
# Obj <- AllReadsSummary(Obj)

  
  
  
  

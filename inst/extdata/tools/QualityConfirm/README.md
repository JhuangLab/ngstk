# QualityConfirm

## Introduction

[QualityConfirm](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/) is a quality control tool for gene panel sequencing data. It relies on *RColorBrewer*, *ggplot2*, and *stringr* R packages. It also uses *samtools* to extract the depth of each base from bam files.

## Guide

Defined environment variable:

- SAMTOOLS, Path of samtools executable file
- REFFA, Path of reference genome

R Dependence:

- ggplot2
- stringr
- RColorBrewer

Extra Dependence:

- samtools
- reference genome


```r
install.packages(c('ggplot2', 'stringr', 'RColorBrewer'))
setwd(dirname(system.file("extdata", "tools/QualityConfirm", package = "ngstk")))
Sys.setenv(SAMTOOLS = "/path/samtools", REFFA='/path/hg19')
source('demo.R')

## Object Initialization
Obj <- new("ReadsCheck", list(panelBed = './example/APL_panel.bed',
           bamDir = './example', resultDir='./example/result'))
```

### Object


*QualityConfirm* defined a S4 object `ReadsCheck`. This object has 15 slots which can be classified into 5 groups:

* Environment
    + `samtools`: the path to *samtools*, default **samtools**.

    + `refhg`: the path to *human genome*, default **hg19.fa**.

* Input Directories or Files
    + `panelBed`: the path to bed files, default scanning the *.bed* file in the current dir.

    + `bamDir`: the path to the directory of all bam files, default current dir.

    + `bamList`: bam files need to analysis, default to scanning all *.bam* files in the `bamDir`.

* Program Parameters
    + `maxDepth`: the parameters used in *samtools*, default **40000**.

    + `bedGap`: the parameters deciding to how to merge the original panel, default **2**.

* Output Directories
    + `resultDir`: the result directory, default **./result**.

    + `depthDir`: the depth data files directory, would set to **resultDir/depth**.

    + `figDir`: the gene coverage diretory, would set to **resultDir/fig**.

    + `sumDir`: the summary diretory, would set to **resultDir/summary**.

    + `mergeBedPath`: the merged panel bed file, would set to **resultDir/merge.panel.bed**.

* Other
    + `currBamNum`: the index of current process bam file, add 1 when finish `SingleReadsSummary`.

    + `status`: the process status of each bam, `ReadsDepth` add 1, `PlotCountFig` add 2, `SingleReadsSummary` add 4.

    + `mergedBed`: the data frame to store the merged panel.

### Functions

*QualityConfirm* has 6 main functions:
* `PanelMerge`: the function to merge panel.

* `PlotBedHist`: the function to plot panel distribution.

* `ReadsDepth`: the function to get depth data files. Give the  `test=TRUE` for program testing without running *samtools* parameter.

* `PlotCountFig`:the function to plot gene coverage figure.

* `SingleReadsSummary`: the function to get the summary for each panel region in a sample.

* `AllReadsSummary`: the function to get the summary of overall.

Every function will return the object with updating. And the inpute is only a defined object.

## Maintainer

- [Tianqi Yan](https://github.com/yantq-sjtu)
- [Jianfeng](https://github.com/yantq-sjtu)

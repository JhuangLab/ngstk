# QualityConfirm

## Introduction

[QualityConfirm](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/) is an quality control tool for gene panel sequencing data.

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
```

## Maintainer

- [Tianqi Yan](https://github.com/yantq-sjtu)
- [Jianfeng](https://github.com/yantq-sjtu)

# [![Build Status](https://travis-ci.org/JhuangLab/ngstk.svg)](https://travis-ci.org/JhuangLab/ngstk) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![codecov](https://codecov.io/github/JhuangLab/ngstk/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/ngstk)

ngstk package
==============

## Introduction

The R package [ngstk](https://github.com/JhuangLab/ngstk) can be used to facilitate the analysis of NGS data, such as visualization, conversion of the data format for WEB service input and another purpose.

In NGS data analysis process, a few of duplicated small scripts, colors theme always be created by us. In most cases, we can't use it in the future if we don't remember when and where the script be created. [ngstk](https://github.com/JhuangLab/ngstk) is a framework that can be used to collect small script, colors theme and other should be packaged material.

The purples of ngstk is that help us to manage those small scripts systematically, store some of the useful material for NGS data analysis.
Especially, data visualization, conversion of data format and various database ID were the mainly mission in the recently development cycle.

## Installation

### CRAN

``` r
#You can install this package directly from CRAN by running (from within R):
install.packages('ngstk')
```

### Github

``` r
# Install the cutting edge development version from GitHub:
# install.packages("devtools")
devtools::install_github("JhuangLab/ngstk")
```

### Zip/Tarball

1. Download the appropriate zip file or tar.gz file from Github
2. Unzip the file and change directories into the configr directory
3. Run `R CMD INSTALL pkg`

## Tools

Some of scripts or tools for NGS data analysis will be included in ngstk package. A defined markdown document will guide you to use it, such as [QualityConfirm](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/README.md).

### QualityConfirm

[QualityConfirm](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/) is an small quality control tool for gene panel sequencing data. Usage of QualityConfirm can be found in [QualityConfirm](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/README.md) and the [demo](https://github.com/JhuangLab/ngstk/tree/master/inst/extdata/tools/QualityConfirm/demo.R) can help you to use it more easily.

## Theme

ngstk provide some of defined colors [theme](https://github.com/JhuangLab/ngstk/blob/master/inst/extdata/config/theme.toml), you can directly download it: https://raw.githubusercontent.com/JhuangLab/ngstk/master/inst/extdata/config/theme.toml.

```toml
Title = "ngstk theme configuration file (colors)"

[default]
colors = ["#0073c3", "#efc000", "#696969",
          "#ce534c", "#7ba6db", "#035892",
					"#052135", "#666633", "#660000", "#990000"]
[red_blue]
colors = ["#c20b01", "#196abd"]

[proteinpaint_mutations]
colors = ["#3987cc", "#ff7f0e", "#db3d3d", "#6633ff",
          "#bbbbbb", "#9467bd", "#998199", "#8c564b", "#819981",
          "#5781ff"]

[proteinpaint_domains]
colors = ["#a6d854", "#8dd3c7", "#fb8072", "#80b1d3", "#bebada", "#e5c494", "#fdb462", "#b3b3b3"]

[proteinpaint_significance]
colors = ["#aaaaaa", "#e99002", "#5bc0de", "#f04124", "#90c3d4", "#f04124", "#43ac6a"]

[adobe_color_cc_1]
colors = ["#FFE350", "#E8740C", "#FF0000", "#9C0CE8", "#0D43FF",
          "#A6B212", "#1991FF", "#ECFF00", "#CC1E14", "#B25C58"]
```

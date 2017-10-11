# [![Build Status](https://travis-ci.org/JhuangLab/ngstk.svg)](https://travis-ci.org/JhuangLab/ngstk) [![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://en.wikipedia.org/wiki/MIT_License) [![codecov](https://codecov.io/github/JhuangLab/ngstk/branch/master/graphs/badge.svg)](https://codecov.io/github/JhuangLab/ngstk)

ngstk package
==============

# Introduction

The R package [ngstk](https://github.com/Miachol/ngstk) can be used to facilitate the analysis of NGS data, such as visualization, conversion of data format for WEB service input and other purpose.

In NGS data analysis process, a few of duplicated small scripts, colors theme always be created by us. In most cases, we can't use it in the future if we don't remember when and where the script be created. [ngstk](https://github.com/Miachol/ngstk) is a framework that can be used to collect small script, colors theme and other should be packaged material.

The purples of ngstk is that help us to manage those small scripts systematically, store some of useful material for NGS data analysis.
Especialy, data visualization, data format conversion and conversion of various database ID were the mainly mission in the recently development cycle.


# Installation

## CRAN
``` r
#You can install this package directly from CRAN by running (from within R):
install.packages('ngstk')
```

## Github
``` r
# Install the cutting edge development version from GitHub:
# install.packages("devtools")
devtools::install_github("Miachol/ngstk")
```

## Zip/Tarball

1. Download the appropriate zip file or tar.gz file from Github
2. Unzip the file and change directories into the configr directory
3. Run `R CMD INSTALL pkg`

# gvmap

## Introduction

gvmap is an R package to draw mutations and fusions heatmap. It relies on *configr*, *rsvg* R package.

## Guide

```r
install.packages(c('configr', 'rsvg', 'devtools'))
setwd(system.file('extdata', 'tools/', package = "ngstk"))
devtools::install_local('gvmap')
library(configr)
library(gvmap)
library(rsvg)

###################################
# step 1, modify your config file
###################################

test_data_dir <- system.file('extdata', 'tools/gvmap/inst/extdata/', package = "ngstk")
setwd(test_data_dir)
config_file <- "test_data/gvmap.config.yaml"
data_file_name <- "test_data/gvmap.test.txt"

data_file <- read.table(data_file_name, header = T, sep = "\t")

########################################
# step 2, set output file and run gvmap
########################################

output_svg_name <- "test_data/gvmap.output.svg"

gvmap(data_file = data_file, config_file, output_svg_name)

########################################
# step 2, save as pdf format
########################################

output_pdf_name <- "test_data/gvmap.output.pdf"

rsvg_pdf(output_svg_name, output_pdf_name)

```

```yaml
########################################
# config parameter
########################################

# 绘图配置文件
map_config:
  sample_col_num: 1     # 样本名所在的列
  group_num: 3          # 图例中分组数目
  group_1:              # group 1的配置文件，分别是行名，颜色主题，对应数据所在列
    - ["Age", "binary_col", 2]
    - ["Gender","binary_col", 3]
    - ["Fusion", "tag_col", 4]
    - ["other_fusion", "pool_col", 5]
  group_2:
    - ["KRAS", "mutation_col", 6]
    - ["P53", "mutation_col", 7]
    - ["NRAS", "mutation_col", 8]
  group_3:
    - ["KRAS-2", "mutation_col", 6]
    - ["P53-1", "mutation_col", 7]
    - ["theme-1", "my_theme_col_1", 4]
    - ["theme-2", "my_theme_col_2", 4]
    - ["NRAS", "mutation_col", 8]
  split_sample:        # 拆分样本，即在图例中添加竖线
    - "C143"
    - "C091"

# 颜色配置文件
color_config:
  bg_col: "#EEEEEE"      # 背景色，默认浅灰色，适用于0和NA
  tag_col: "#696969"     # 目标颜色，默认深灰色
  binary_col: ["#196ABD", "#C20B01"]    # 二元色，默认蓝色和红色，可用于绘制性别这种类型的数据
  white_col: "#FFFFFF"
  black_col: "#000000"
  mutation_col:          # 突变色，每种颜色对应一种突变，具体参见测试文件的中的KRAS,P53,NRAS
    missense: "#3987CC"
    frameshift: "#DB3D3D"
    nonsense: "#FF7F0E"
    protein_del: "#7F7F7F"
    protein_ins: "#8C564B"
    one_hit: "#819981"
    tow_hit: "#499E49"
    three_hit: "#237023"
  pool_col: ["#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"]   # 颜色池，适用于有多个不同元素的列
  my_theme_col_1: "#00FFFF"   # 自定义颜色，可以无限增加，但是目前只能有一种。参见测试文件中的theme-1
  my_theme_col_2: "#CCCC33"   # 自定义颜色，可以无限增加，但是目前只能有一种。参见测试文件中的theme-2

# 绘图配置文件
rect_config:
  plot_width: 1600      # 画布的宽
  plot_height: 1200     # 画布的高
  width: 14             # 图例中每个小方块的宽
  height: 16            # 图例中每个小方块的高
  stroke_width: 0.5     # 图例中每个小方块的描线的粗细
  span: 6               # 两组之间的距离
  frame: True           # 是否加黑色的方框
  leg_font_size: 12     # 图例左边的字体大小
  sample_font_size: 12  # 图例中样本的字体大小
  font_family: "Arial"  # 图例中所有文字的字体
```

## Maintainer

[Yuting Dai](https://github.com/ytdai)

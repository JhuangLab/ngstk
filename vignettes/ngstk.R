## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
library(ngstk)

## ------------------------------------------------------------------------
demo_file <- system.file("extdata", "demo/proteinpaint/muts2pp_iseq.txt", package = "ngstk")
input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
disease <- "T-ALL"
input_data <- data.frame(input_data, disease)
input_data$disease <- as.character(input_data$disease)

# Convert mutations data to proteinpaint input
result <- muts2pp(input_data, input_type = "iseq")
head(result)
# Convert mutations data to cbioportal input
result <- muts2mutation_mapper(input_data, input_type = "iseq")
head(result)
result <- muts2oncoprinter(input_data, input_type = "iseq")
head(result)

demo_file <- system.file('extdata', 'demo/proteinpaint/fusions2pp_fusioncatcher.txt', package = 'ngstk')
input_data <- read.table(demo_file, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
disease <- 'B-ALL'
sampletype <- 'diagnose'
input_data <- data.frame(input_data, disease, sampletype)
input_data$disease <- as.character(input_data$disease)
# Convert fusions data to proteinpaint input
result <- fusions2pp(input_data, input_type = 'fusioncatcher')
head(result)

## ------------------------------------------------------------------------
a <- data.frame(col1=1:6, col2=2:7)
b <- data.frame(col1=6:11, col2=1:6)
file_a <- paste0(tempfile(), '_abcd')
file_b <- paste0(tempfile(), '_abcd')
write.table(a, file_a, sep = '\t', row.names = FALSE)
write.table(b, file_b, sep = '\t', row.names = FALSE)
input_files <- c(file_a, file_b)
x1 <- merge_table_files(input_files = input_files)
head(x1)
x2 <- merge_table_files(files_dir = tempdir(), pattern = '.*_abcd$')
head(x2)
outfn = tempfile()
x3 <- merge_table_files(files_dir = tempdir(), pattern = ".*_abcd$", outfn = outfn)
head(read.table(outfn, sep = "\t", header = TRUE))

## ------------------------------------------------------------------------
demo_file <- system.file("extdata", "demo/proteinpaint/fusions2pp_fusioncatcher.txt", package = "ngstk")
input_data <- read.table(demo_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
# Get data subset according the defined rule
mhander_extra_params = list(gene_5 = 1, gene_3 = 2, any_gene = "TCF3", fusions_any_match_flag = TRUE)
result_1 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
head(result_1)

mhander_extra_params = list(gene_3 = 2, right_gene = "GYPA", fusions_right_match_flag = TRUE)
result_2 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
head(result_2)

mhander_extra_params = list(gene_5 = 1, left_gene = "GYPA", fusions_left_match_flag = TRUE)
result_3 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
head(result_3)

mhander_extra_params = list(gene_5 = 1, gene_3 = 2, left_gene = "GYPE", right_gene = "GYPA", fusions_full_match_flag = TRUE)
result_4 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
head(result_4)

mhander_extra_params = list(gene_5 = 1, gene_3 = 2, left_gene = "GYPE", right_gene = "GYPA", fusions_anyfull_match_flag = TRUE)
result_5 <- fusions_filter(input_data, mhander_extra_params = mhander_extra_params)
head(result_5)


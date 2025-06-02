#!/usr/bin/env Rscript

library(tidyverse)
library(collapse)
library(arrow)
library(data.table)
library(optparse)

option_list <- list(
  make_option(c("-i", "--input"),
              type="character",
              help="Input RDS file path")
)

opt <- parse_args(OptionParser(option_list=option_list))

if (is.null(opt$input)) {
  stop("Please provide an input file path using -i or --input")
}

tryCatch({
  dat1 <- readRDS(opt$input)

  metabolite_cols <- grep("^X", names(dat1), value = TRUE)
  metabolite_data <- dat1[, metabolite_cols]

  cor_matrix <- cor(metabolite_data)

  cor_pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  cor_results <- data.frame(
    metabolite1 = rownames(cor_matrix)[cor_pairs[,1]],
    metabolite2 = colnames(cor_matrix)[cor_pairs[,2]],
    correlation = cor_matrix[cor_pairs]
  )

  high_cor <- cor_results[cor_results$correlation > 0.9, ]

  high_cor$mass1 <- as.numeric(str_extract(high_cor$metabolite1, "(?<=X)\\d+\\.\\d+"))
  high_cor$mass2 <- as.numeric(str_extract(high_cor$metabolite2, "(?<=X)\\d+\\.\\d+"))

  saveRDS(high_cor,
          paste0("2.",basename(opt$input),"_high_cor.rds"))

  metabolites_to_remove <- unique(
    ifelse(high_cor$mass1 >= high_cor$mass2,
           as.character(high_cor$metabolite1),
           as.character(high_cor$metabolite2))
  )

  dat1_filtered <- dat1[, !names(dat1) %in% metabolites_to_remove]

  saveRDS(metabolites_to_remove,
          paste0("2.",basename(opt$input),"_metabolites_removed.rds"))

  write_parquet(dat1_filtered,
                paste0("2.",basename(opt$input),"_metabolites_filtered.parquet"))


}, error = function(e) {
  stop(paste("Error in processing:", e$message))
})

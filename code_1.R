library(readxl)
matrices <- read_excel("/Users/nelly/Desktop/problem_1/matrices.xlsx")

gene_count <- c()
for(i in 2:ncol(matrices)) 
{
  gene_count[colnames(matrices)[i]] <- sum(matrices[,i])
}

library(tidyverse)
gene_count <- as.data.frame(gene_count)
gene_count <- gene_count %>% rownames_to_column("gene_names")

library(stringr)
gene_count$alt <- str_split_fixed(gene_count$gene_names, "\\.",2)[,1]
gene_count$gene_names <- str_split_fixed(gene_count$gene_names, "\\.",2)[,2]

library(tidyverse)
fin <- gene_count %>%
  pivot_wider(names_from = gene_names, 
              values_from = gene_count)

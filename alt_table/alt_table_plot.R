library(readxl)

matrices <- read_excel("/Users/nelly/Desktop/alt_table/matrices.xlsx", na = "NA")  


gene_count <- c()

for (i in 2:ncol(matrices)) {
  gene_count[colnames(matrices)[i]] <- sum(matrices[,i], na.rm = T)
}

library(tidyverse)

gene_count <- as.data.frame(gene_count)
gene_count <- gene_count %>% rownames_to_column("gene_names")

library(stringr)

gene_count$alt <- str_split_fixed(gene_count$gene_names, "\\.",2)[,1]
gene_count$gene_names <- str_split_fixed(gene_count$gene_names, "\\.",2)[,2]

library(tidyverse)

alteration_table <- gene_count %>%
  pivot_wider(names_from = gene_names, 
              values_from = gene_count)

library(tidyverse)

ggplot(gene_count) + 
  geom_jitter(aes(x = alt, y = gene_count, color = pathway), width = 0.2, height = 0, size = 2) + 
  scale_y_log10()

only_gene_and_path <- genes_without_character0 %>% select(7, 13)
only_gene_and_path <- unique(only_gene_and_path[c("gene", "pathway")])


gene_count$pathway <- sapply(gene_count$gene_names, function(x) {
  only_gene_and_path$pathway[unique(which(x == only_gene_and_path$gene))]})
                                    
                                    
gene_count <- gene_count[- which(gene_count$pathway == "list()"),]
gene_count$pathway <- unlist(gene_count$pathway)


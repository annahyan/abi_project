library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(readxl)
library(here)
library(networkD3)

all_data <- read_excel("Plotting", "COSMIC_1001_Cell_lines_mutational_signatures.xlsx", 
                       sheet = "COSMIC CellLines Signatures")[c(-1, -3)]


all_data <- read_excel(here("plotting", "COSMIC_1001_Cell_lines_mutational_signatures.xlsx"), 
                       sheet = "COSMIC CellLines Signatures")[c(-1, -3)]

cancers = unique(all_data$`Cancer Type used in SigProfiler`)
sankey_data = matrix(nrow = length(cancers), ncol = ncol(all_data) - 1)
rownames(sankey_data) = cancers
colnames(sankey_data) = colnames(all_data)[-1]

for(i in cancers) {
  ind = which(all_data$`Cancer Type used in SigProfiler` == i)
  df <- all_data[ind,]
  sankey_data[i,] = colSums(df[,-1] > 0)
}

data = as.data.frame(sankey_data)

# I need a long format
data_long <- data %>%
      rownames_to_column %>%
      gather(key = 'key', value = 'value', -rowname) %>%
      filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
nodes

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = data_long, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)





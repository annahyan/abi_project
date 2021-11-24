# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(readxl)
library(tidyr)

# Upload a dataset
data <- read_excel("/Users/nelly/Desktop/COSMIC_1001_Cell_lines_mutational_signatures.xlsx", 
                   sheet = "COSMIC CellLines Signatures")[c(-1,-3)]

# Long format of data
data_long <- data %>%
  gather(key = 'mutational_signatur', value = 'value', c(-`Cancer Type used in SigProfiler`)) %>%
  filter(value > 0)

# Box plot
data_long %>%
  ggplot(aes(x = mutational_signatur, y = value, fill = mutational_signatur)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size=0.4, alpha = 0.9) +
  scale_y_log10() +
  theme_ipsum(base_size = 13, axis_title_size = 13) +
  xlab("Mutational signatur") +
  ylab("Count of mutation") +
  theme(legend.position = "none",
        plot.title = element_text(size = 11),
        text = element_text(size = 40),
        axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("boxplot.png", width = 12, height = 8, bg = "white")


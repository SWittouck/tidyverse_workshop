library(tidyverse)
library(tidyamplicons)

# exercise 1
load("data/carrots.rda")

# exercise 2
carrots %>%
  add_lib_size() %>%
  samples() %>%
  ggplot(aes(x = lib_size)) +
  geom_line(stat = "density") +
  geom_rug()

# exercise 3
carrots %>%
  filter_samples(type == "FP") %>%
  add_diversity_measures() %>%
  samples() %>%
  ggplot(aes(x = Day, y = div_inv_simpson)) +
  geom_boxplot(alpha = 0.5) + 
  geom_jitter()

# exercise 4
carrots %>%
  add_pcoa() %>%
  samples() %>%
  ggplot(aes(x = pcoa1, y = pcoa2, col = type)) +
  geom_point()

# exercise 5
carrots %>%
  filter_taxa(phylum == "Firmicutes") %>%
  get_bar_plot()

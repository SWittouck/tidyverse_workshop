library(tidyverse)

# 1) Import
otu_table <- read_tsv("data/otutable.tsv")

# 2) Tidy
otu_table_tidy <- otu_table %>%
  gather(key = "sample", value = "abundance", contains("Sample"))

# 3) Relative abundances 
otu_table_tidy <- otu_table_tidy %>%
  group_by(sample) %>%
  mutate(rel_abundance = abundance / sum(abundance)) %>%
  ungroup()

# 4) Filter taxa
otu_table_filtered <- otu_table_tidy %>%
  group_by(taxon) %>%
  mutate(taxon_av_rel_ab = mean(rel_abundance)) %>%
  ungroup() %>%
  filter(taxon_av_rel_ab >= 0.01)

# 5) Tile plot
ggplot(otu_table_filtered, aes(x = sample, y = taxon, fill = rel_abundance)) +
  geom_tile()
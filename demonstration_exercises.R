library(tidyverse)
library(stringr)
library(lintr)

# download data

eukaryotes <- read_tsv(
  file = "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/eukaryotes.txt", 
  na = c("", "na", "-")
)

# format and subset data

names_new <- names(eukaryotes) %>% 
  str_replace_all("[#%()]", "") %>% 
  str_replace_all("[ /]", "_") %>%
  str_to_lower

animals <- eukaryotes %>%
  filter(Group == "Animals") %>%
  set_names(names_new)

# save and read

if (! dir.exists("data")) dir.create("data")
if (! dir.exists("results")) dir.create("results")
write_tsv(animals, "data/animals.tsv")
rm(list = ls())

# 2. introduction to ggplot2

## Read in data
animals <- read_tsv("data/animals.tsv")

## Explore dataset
glimpse(animals)

## First plot
ggplot(animals, aes(x = subgroup)) +
  geom_bar() +
  coord_flip()

## Second plot
ggplot(animals, aes(x = subgroup, y = size_mb)) +
  geom_boxplot() +
  coord_flip()

## Third plot
ggplot(animals, aes(x = gc, y = size_mb)) +
  geom_point()

## Third plot + additional aesthetic
ggplot(animals, aes(x = gc, y = size_mb, color = subgroup)) +
  geom_point()

## Third plot + additional facet layer
ggplot(animals, aes(x = gc, y = size_mb, color = subgroup)) +
  geom_point() +
  facet_wrap(~subgroup)

# Save last plot
ggsave("results/scatterplot.png", units = "cm", width = 20, height = 15)

# 3. introduction to dplyr

## select
animals2 <- select(animals, organism_name, subgroup, size_mb) 

## mutate
animals2 <- mutate(animals, size_kb = size_mb * 1000) 

## filter
animals2 <- filter(animals, subgroup == "Mammals")

## arrange
animals2 <- arrange(animals, - size_mb)

## summarize
summarize(animals, n = n(), mean_genome_size = mean(size_mb)) 

## group-wise mutate
animals2 <- group_by(animals, organism_name) 
animals2 <- mutate(animals2, genome_size_z = (size_mb - mean(size_mb)) / sd(size_mb)) 
animals2 <- ungroup(animals2)

## group-wise summarize
animals2 <- group_by(animals, organism_name) 
animals2 <- summarize(animals2, n = n(), mean_genome_size = mean(size_kb)) 

## pipe operator
animals2 <- animals %>%
  filter(! is.na(subgroup)) %>%
  group_by(subgroup) %>%
  summarize(n = n(), mean_genome_size = mean(size_mb)) %>%
  arrange(- mean_genome_size)

# 4. introduction to tidy data

animals %>%
  select(assembly_accession, subgroup, genes, proteins) %>%
  gather(key = "feature", value = "count", genes, proteins) %>%
  ggplot(aes(x = feature, y = count, group = assembly_accession)) +
  geom_line() +
  facet_wrap(~ subgroup, scales = "free_y")

# 5. Additional ggplot2 tweaking

## Scatterplot from 2
ggplot(animals, aes(x = gc, y = size_mb, color = subgroup)) +
  geom_point() +
  facet_wrap(~subgroup) +
  xlab("GC-content (%)") + 
  ylab("Genome size (Mb)") + 
  ggtitle("NCBI Eukaryotic Genome Database") +
  scale_y_log10() + 
  scale_colour_brewer(palette = "Paired") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, face = "italic", colour = "darkgrey"),
        
        panel.grid.minor = element_blank())
  

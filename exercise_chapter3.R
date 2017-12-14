library(tidyverse)

# 1) Import file
sample_data <- read_tsv("data/sampledata.tsv")

# 2) Filter
sample_data_filtered <- sample_data %>%
  filter(! is.na(bmi_group), ! is.na(nationality)) %>%
  select(bmi_group, nationality)

# 3) Count
sample_data_summary <- sample_data_filtered %>%
  group_by(nationality, bmi_group) %>%
  summarize(count = n()) %>%
  arrange(nationality, - count) 

# 4) Barpot
ggplot(sample_data_summary, aes(x = nationality, y = count, fill = bmi_group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired")

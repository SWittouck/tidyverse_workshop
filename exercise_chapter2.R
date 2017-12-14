library(tidyverse)

# Exercise chapter 2

## 1. Read in data
sampledata <- read_tsv("data/sampledata.tsv")

## 2. Explore the dataset
sampledata

## 3. Plot the amount of males and females in this study using a barchart
ggplot(sampledata, aes(x = gender)) +
  geom_bar()

## 4. Do the same but for the nationality of the participants
ggplot(sampledata, aes(x = nationality)) +
  geom_bar() +
  coord_flip()

## 5. Create a boxplot showing the age distribution of each nationality. Use the fill aesthetic to make it a little bit more colorful
ggplot(sampledata, aes(x = nationality, y = age, fill = nationality)) +
  geom_boxplot() 

## 6. Add an extra layer to 5. with plotting points over the boxplot. Remove that layer again and explore the difference with geom_jitter()
ggplot(sampledata, aes(x = nationality, y = age, fill = nationality)) +
  geom_boxplot() +
  geom_jitter()

## 7. Advanced: Plot the age density of all participants coloured by gender, faceted per nationality.
ggplot(sampledata, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~nationality, nrow = 7) +
  geom_rug()

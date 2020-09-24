# Minor script to find PHYLACINE megafauna facts
library(tidyverse)

df <- read_csv("builds/data.csv", col_types = cols())

tot <- nrow(df)

df %>% 
  mutate(EX = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  filter(Mass.g >= 45000) %>% 
  count(EX)

# Pct extinct megafauna of all terrestrial mammals
round(175/tot * 100, 1)

df %>% 
  mutate(EX = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  count(EX)

# Pct extinct mammals of all terrestrial mammals
round(336/tot * 100, 1)


df %>% 
  mutate(EX = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  filter(Mass.g >= 45000) %>% 
  count(EX)

# Pct extinct mammals of all terrestrial mammals
round(175/c(175+152) * 100, 1)

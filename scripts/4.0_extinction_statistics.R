# Minor script to find PHYLACINE megafauna facts
# 29/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)

# Load filtered data
df <- read_csv("builds/data.csv", col_types = cols())

# Total number of mammals
(tot <- nrow(df))

# Extinct fauna
df %>% 
  mutate(Extinct = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  count(Extinct)

# Pct extinct mammals of all terrestrial mammals
signif(333/tot * 100, 2)

# Extinct megafauna
df %>% 
  mutate(Extinct = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  filter(Mass.g >= 45000) %>% 
  count(Extinct)

# Pct extinct mammals of all terrestrial mammals
signif(173/c(173+151) * 100, 2)

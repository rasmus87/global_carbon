# Minor script to find PHYLACINE megafauna facts
# Can be run as a separate section
# 29/07-2021 Rasmus Ø Pedersen

# Load libraries
library(raster)
library(tidyverse)
library(Matrix)

# Load filtered data
df <- read_csv("builds/data.csv", col_types = cols())


# Overall extinction statistics -------------------------------------------

# Total number of mammals
paste("Number of terrestrial mammals (in this study):", nrow(df))

# Fraction of terrestrial fauna extinct [1 group]
df %>% 
  mutate(Extinct = IUCN.Status.1.2 %in% c("EX", "EP")) %>% 
  count(Extinct) %>% 
  summarise(n.extinct = n[which(Extinct)],
            n.total = sum(n),
            pct.extinct = signif(n.extinct/n.total*100, 2))

# Fraction of terrestrial fauna extinct by size class [2 groups]
df %>% 
  mutate(Extinct = IUCN.Status.1.2 %in% c("EX", "EP"),
         Mass.group = cut(Mass.g/1000, c(0, 45, Inf), right = FALSE)) %>% 
  count(Extinct, Mass.group) %>% 
  group_by(Mass.group) %>% 
  summarise(pct.extinct = signif(n[which(Extinct)]/sum(n)*100, 2))

# Fraction of terrestrial fauna extinct by size class [3 groups]
df %>% 
  mutate(Extinct = IUCN.Status.1.2 %in% c("EX", "EP"),
         Mass.group = cut(Mass.g/1000, c(0, 45, 1000, Inf), right = FALSE)) %>% 
  count(Extinct, Mass.group) %>% 
  group_by(Mass.group) %>% 
  summarise(pct.extinct = signif(n[which(Extinct)]/sum(n)*100, 2))




# Australasia extinction statistics ---------------------------------------

# Load realms and filter to Australasia
base.map <- raster("builds/base_map.tif")
wwf.realm <- base.map
wwf.realm[] <- raster("data/wwf_terr_ecos_realm_raster.tif")[]
australasia <- which(wwf.realm[] == 8)

# Load range maps:
current.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")
present.natural.maps <- read_rds("builds/present.natural.maps.filtered.edge.lim.rds")

# Filter for current species
cu <- rowSums(current.maps[, australasia]) %>% 
  as_tibble(rownames = "Binomial.1.2") %>% 
  filter(value > 0) %>% 
  transmute(Binomial.1.2, CU = 1)

# Filter for present natural species
pn <- rowSums(present.natural.maps[, australasia]) %>% 
  as_tibble(rownames = "Binomial.1.2") %>% 
  filter(value > 0) %>% 
  transmute(Binomial.1.2, PN = 1)

# Combine them
aus.df <- df %>%
  transmute(Family.1.2, Binomial.1.2, Diet.Plant, Mass.kg = Mass.g/1000) %>% 
  left_join(cu, by = "Binomial.1.2") %>% 
  left_join(pn, by = "Binomial.1.2") %>% 
  filter(!is.na(CU) | !is.na(PN)) %>% 
  mutate(Extinct = ifelse(is.na(CU), TRUE, FALSE),
         CU = NULL,
         PN = NULL)

# Australasian fauna by extinction and size class
aus.df %>% 
  filter(Diet.Plant >= 90) %>% 
  mutate(weight = cut(Mass.kg, 
                      breaks = c(0, 45, Inf),
                      right = FALSE)) %>% 
  count(weight, Extinct)

# Summary of weights for Australasian fauna
aus.df %>% 
  filter(Diet.Plant >= 90,
         Mass.kg >= 45) %>% 
  group_by(Extinct) %>% 
  summarise(min = min(Mass.kg),
            max = max(Mass.kg), 
            median = median(Mass.kg))


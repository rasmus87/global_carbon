# Make experimental plots

# Load libraries
library(tidyverse)
library(tictoc)
library(Matrix)
library(raster)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(viridis)  # better colors for everyone
library(gridExtra)



# Load data ---------------------------------------------------------------

# Load traits
df <- read_csv("builds/data.csv", col_types = cols())

# Load density data
density.samples <- read_csv("builds/sampled.density.distribution.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]
# Alignment sanity check 
stopifnot(all(density.samples$Binomial.1.2 == df$Binomial.1.2))

# Transform to biomass
mass.samples <- density.samples[, -1] * as.numeric(df$Mass.g)/1e6 # [Mg / km2]

mass.samples <- mass.samples %>% as_tibble() %>%  mutate(Binomial.1.2 = df$Binomial.1.2, .before = "sample.1")


# Load consumption data
consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]

# Estimate this in terms of plant consumption
Q.plant <- consumption.samples[, -1] * as.numeric(df$Diet.Plant)/100
Q.plant <- Q.plant %>% as.matrix() %>% .[] / 10^3 # [MgC / (km2 * year)]

Q.plant <- Q.plant %>% as_tibble %>% mutate(Binomial.1.2 = df$Binomial.1.2, .before = "sample.1")

# Confirm aligment
# qplot(Q.plant[1, -1] %>% as.numeric(),
#       mass.samples[1, -1] %>% as.numeric())



# Attempt at filtering extreme values
mass.samples.filtered <- mass.samples %>%
  pivot_longer(-Binomial.1.2, names_to = "sample") %>%
  group_by(Binomial.1.2) %>%
  filter(value >= quantile(value, .025),
         value <= quantile(value, .975)) %>%
  ungroup() %>%
  filter(!is.na(value))

Q.plant.filtered <- Q.plant %>%
  pivot_longer(-Binomial.1.2, names_to = "sample") %>% 
  left_join(mass.samples.filtered[1:2], ., by = c("Binomial.1.2", "sample"))


# Renaming things
mass.samples <- mass.samples.filtered %>% 
  mutate(sample = rep(paste0("sub.sample.", 1:950), nrow(df))) %>%
  pivot_wider(values_from = value, names_from = sample)

Q.plant <- Q.plant.filtered %>% 
  mutate(sample = rep(paste0("sub.sample.", 1:950), nrow(df))) %>%
  pivot_wider(values_from = value, names_from = sample)


# # Confirm aligment
# qplot(Q.plant[1, -1] %>% as.numeric(),
#       mass.samples[1, -1] %>% as.numeric())


# Load species maps and calculate consumption maps ------------------------

# Current maps
current.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")
current.maps <- current.maps[df$Diet.Plant > 0, ]
present.natural.maps <- read_rds("builds/present.natural.maps.filtered.edge.lim.rds")
present.natural.maps <- present.natural.maps[df$Diet.Plant > 0, ]


# Base map
base.map <- raster("builds/base_map.tif")

# New land map
land <- raster("builds/land.tif")
land.df <- as(land, "SpatialPixelsDataFrame") # [MgC / km2 / year]
land.df <- as_tibble(land.df)
land.df$land <- NA


# Only keep land
land.sparse <- land[]
land.sparse[which(is.na(land[]))] <- 0
land.sparse <- as(land.sparse[], "sparseVector")
current.maps <- t(t(current.maps) * land.sparse)
present.natural.maps <- t(t(present.natural.maps) * land.sparse)


# Create ggplot theme
theme_R <- function() {
  theme_bw() %+replace% 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
}

all.cells <- base.map
all.cells[] <- 1:prod(dim(base.map))
names(all.cells) <- "cell"
eco.units.cells <- all.cells %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble %>% 
  inner_join(eco.units)

eco.units.cells %>% count(eco.unit)

# Test region
select.cells <- test$cell
# All land
# select.cells <- which(land[] == 1)

realms <- unique(eco.units.cells$realm)
i <- realms[1]
rm(res.delta.full)
rm(res.full)
for(i in realms) {
  select.cells <- eco.units.cells %>%
    filter(realm == i) %>% 
    pull(cell)
  
  # Subset current maps
  subset <- current.maps[, select.cells]
  subset <- subset[rowSums(subset) > 0, ]
  
  subset.df <- subset %>% 
    as.matrix() %>%
    as_tibble %>%
    mutate(Binomial.1.2 = rownames(subset), .before = "V1") %>% 
    pivot_longer(-Binomial.1.2, names_to = "cell") %>% 
    filter(value > 0) %>% 
    rename(n.species = value)
  
  res.Q <- subset.df %>% 
    left_join(Q.plant, by = "Binomial.1.2") %>% 
    mutate(Binomial.1.2 = NULL) %>% 
    mutate(across(starts_with("Q.sample"), ~ . * n.species)) %>% # Downscale edge cells
    group_by(cell) %>% 
    summarise_all(sum) %>% 
    pivot_longer(-c(cell, n.species), values_to = "Q")
  
  res.mass <- subset.df %>% 
    left_join(mass.samples, by = "Binomial.1.2") %>% 
    mutate(Binomial.1.2 = NULL) %>% 
    mutate(across(starts_with("V"), ~ . * n.species)) %>% # Downscale edge cells
    group_by(cell) %>% 
    summarise_all(sum) %>% 
    pivot_longer(-c(cell, n.species), values_to = "mass")
  
  res.cu <- bind_cols(res.Q, res.mass["mass"])
  
  
  # PN
  subset <- present.natural.maps[, select.cells]
  subset <- subset[rowSums(subset) > 0, ]
  
  subset.df <- subset %>% 
    as.matrix() %>%
    as_tibble %>%
    mutate(Binomial.1.2 = rownames(subset), .before = "V1") %>% 
    pivot_longer(-Binomial.1.2, names_to = "cell") %>% 
    filter(value > 0) %>% 
    rename(n.species = value)
  
  res.Q <- subset.df %>% 
    left_join(Q.plant, by = "Binomial.1.2") %>% 
    mutate(Binomial.1.2 = NULL) %>%
    mutate(across(starts_with("Q.sample"), ~ . * n.species)) %>% # Downscale edge cells %>% 
    group_by(cell) %>% 
    summarise_all(sum) %>% 
    pivot_longer(-c(cell, n.species), values_to = "Q")
  
  res.mass <- subset.df %>% 
    left_join(mass.samples, by = "Binomial.1.2") %>% 
    mutate(Binomial.1.2 = NULL) %>% 
    mutate(across(starts_with("V"), ~ . * n.species)) %>% # Downscale edge cells %>% 
    group_by(cell) %>% 
    summarise_all(sum) %>% 
    pivot_longer(-c(cell, n.species), values_to = "mass")
  
  res.pn <- bind_cols(res.Q, res.mass["mass"])
  
  res <- bind_rows(mutate(res.pn, period = "Pres.nat."), mutate(res.cu, period = "Current")) %>% 
    bind_cols(realm = i)
  if(!exists("res.full")) {
    res.full <- res 
  } else {
    res.full <- bind_rows(res.full, res)
  }
  
  res.delta <- full_join(res.pn, res.cu, by = c("cell", "name"))
  res.delta <- res.delta %>% mutate(Q.delta = Q.x - Q.y, 
                                    mass.delta = mass.x - mass.y, 
                                    realm = i)
  if(!exists("res.delta.full")) {
    res.delta.full <- res.delta 
  } else {
    res.delta.full <- bind_rows(res.delta.full, res.delta)
  }
}

res.delta.full <-res.delta.full %>% mutate(Q.pct.change = Q.y/Q.x * 100, mass.pct.change = mass.y/mass.x * 100)
  

ggplot(res, aes(mass, Q)) +
  facet_wrap(~period) + 
  geom_hex(bins = 60) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()

sample <- sample_n(res, 1e4)
ggplot(sample, aes(mass, Q, col = period)) +
  geom_point(alpha = .25) +
  geom_smooth() +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


# ggplot(res, aes(mass, Q, col = period)) +
#   geom_density_2d() +
#   theme_R() +
#   scale_x_log10() +
#   scale_y_log10()

library(ggnewscale)

library(scico)

scico(3, palette = 'roma')


x <- sample_n(res.full, 1e5)
ggplot(data = x, aes(mass, Q)) +
  stat_density2d(aes(fill = period, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  scale_fill_manual(values = c("Current" = "#ffa6a1", "Pres.nat." = "#4567a3")) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()




# Delta -------------------------------------------------------------------

# Delta change
y <- sample_n(res.delta.full, 1e5)
ggplot(data = y, aes(mass.delta, Q.delta)) +
  stat_density2d(aes(fill = realm, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()

ggplot(data = y, aes(mass.delta, Q.delta)) +
  stat_density2d(aes(fill = realm, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  geom_smooth(aes(col = realm)) +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


# Fraction change
ggplot(data = y, aes(mass.pct.change, Q.pct.change)) +
  stat_density2d(aes(fill = realm, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  geom_smooth(aes(col = realm)) +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


ggplot(data = y, aes(mass.pct.change, Q.pct.change)) +
  stat_density2d(aes(fill = realm, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


ggplot(y, aes(mass.pct.change, Q.pct.change, col = realm)) +
  geom_point(alpha = .25) +
  geom_smooth() +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()



y.filt <- y %>% filter(mass.pct.change <= 100, Q.pct.change <= 100)
y.filt.centroids <- y.filt %>% group_by(realm) %>% summarise(mass.pct.change = median(mass.pct.change), Q.pct.change = median(Q.pct.change))
p <- ggplot(y.filt, aes(mass.pct.change, Q.pct.change, col = realm)) +
  geom_point(alpha = .15, size = 1, stroke = 0, shape = 16) +
  geom_smooth(size = 1.2) +
  geom_smooth(size = 0.2, col =  "black", aes(fill = realm), se = FALSE, show.legend = FALSE) +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  geom_point(data = y.filt.centroids) +
  geom_point(data = y.filt.centroids, shape = 10, col = "black", aes(fill = realm)) +
  theme_R() +
  xlab("Total CU mass / Total PN mass %") +
  ylab("Total CU consumption / Total PN consumption %")
ggsave("./output/test1.png", p, width = 183, height = 137, units = "mm", dpi = 600)


ggplot(y.filt, aes(mass.pct.change, Q.pct.change, col = realm, fill = realm)) +
  # stat_density2d(aes(alpha = ..level..),
  #                geom = "polygon",
  #                col = "#dddddd") +
  geom_smooth() +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


ggplot(y.filt, aes(mass.pct.change, Q.pct.change)) +
  stat_density2d(aes(fill = realm, alpha = ..level..),
                 geom = "polygon",
                 col = "#dddddd") +
  scale_fill_manual(name = "Realm", values = cols, breaks = breaks) +
  theme_R() +
  scale_x_log10() +
  scale_y_log10()


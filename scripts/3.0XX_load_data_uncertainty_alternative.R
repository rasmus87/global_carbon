# Load data and packages used in models and graphing
# 27/07-2021 Rasmus Ø Pedersen

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

# Make a subset excluding strict non herbivores
# Strict non-herbivores
no.herb <- which(df$Diet.Plant == 0)
df.herb <- df[-no.herb, ]

# Load consumption data
consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]

# Alignment sanity check 
stopifnot(all(names(consumption.samples) == df.herb$Binomial.1.2))

# Summarize consumption
consumption.summary <- tibble(Binomial.1.2 = names(consumption.samples),
                              mean = colMeans(consumption.samples),
                              geo.mean = apply(consumption.samples, 2, FUN = function(.) exp(mean(log(.)))),
                              median = apply(consumption.samples, 2, median),
                              ci.lw = apply(consumption.samples, 2, quantile, probs = .025),
                              ci.hi = apply(consumption.samples, 2, quantile, probs = .975))

# Load density data
density.samples <- read_csv("builds/sampled.density.distribution.csv", col_types = cols()) # [kgC / (km2 * year)]

# Alignment sanity check 
stopifnot(all(names(density.samples) == df$Binomial.1.2))

# Transform to biomass [Mg / km2]
mass.samples <- sweep(density.samples, MARGIN = 2, df$Mass.g/1e6, `*`)

# # Summarize mammal biomass
mass.summary <- tibble(Binomial.1.2 = names(mass.samples),
                       mean = colMeans(mass.samples),
                       geo.mean = apply(mass.samples, 2, FUN = function(.) exp(mean(log(.)))),
                       median = apply(mass.samples, 2, median),
                       ci.lw = apply(mass.samples, 2, quantile, probs = .025),
                       ci.hi = apply(mass.samples, 2, quantile, probs = .975))




# Load species maps and calculate consumption maps ------------------------

# Base map
base.map <- raster("builds/base_map.tif")
temp.map <- base.map

# New land map
land <- raster("builds/land.tif")
land.v <- land[]
land.i <- which(!is.na(land.v))
land.df <- as(land, "SpatialPixelsDataFrame") # [MgC / km2 / year]
land.df <- as_tibble(land.df)
land.df$land <- NA


# Current maps
current.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")
current.maps.herb <- current.maps[-no.herb, ]

# Present natural maps
present.natural.maps <- read_rds("builds/present.natural.maps.filtered.edge.lim.rds")
present.natural.maps.herb <- present.natural.maps[-no.herb, ]


# Calculate total consumption per grid cell per map -------------------------------

timestamp()
tic()
# Calculate a current consumption map for every sample for every species
current.consumption.samples.map <- apply(consumption.samples[1:1000, ], 1, function(.) . * current.maps.herb[, ]) 
current.consumption.samples.map <- c(current.consumption.samples.map, apply(consumption.samples[1:1000 + 1000, ], 1, function(.) . * current.maps.herb[, ]))
current.consumption.samples.map <- c(current.consumption.samples.map, apply(consumption.samples[1:1000 + 2000, ], 1, function(.) . * current.maps.herb[, ]))
gc()
toc()
tic()
# Sum total current consumption for each sample
current.consumption.maps <- sapply(current.consumption.samples.map, colSums)
rm(current.consumption.samples.map)
gc()
# Remove oceans
current.consumption.maps.land <- current.consumption.maps[land.i, ] / 1000
toc()

timestamp()
tic()
# Calculate a present natural consumption map for every sample for every species
present.natural.consumption.samples.map <- apply(consumption.samples[1:1000, ], 1, function(.) . * present.natural.maps.herb[, ]) 
present.natural.consumption.samples.map <- c(present.natural.consumption.samples.map, apply(consumption.samples[1:1000 + 1000, ], 1, function(.) . * present.natural.maps.herb[, ]))
present.natural.consumption.samples.map <- c(present.natural.consumption.samples.map, apply(consumption.samples[1:1000 + 2000, ], 1, function(.) . * present.natural.maps.herb[, ]))
gc()
toc()
tic()
# Sum total present natural consumption for each sample
present.natural.consumption.maps <- sapply(present.natural.consumption.samples.map, colSums)
rm(present.natural.consumption.samples.map)
gc()
# Remove oceans
present.natural.consumption.maps.land <- present.natural.consumption.maps[land.i, ] / 1000
toc()


# Calculate current summary statistics
tic()
current.consumption.mean <- apply(current.consumption.maps.land, 1, mean)
current.consumption.geo.mean <- apply(current.consumption.maps.land, 1, FUN = function(.) exp(mean(log(.))))
current.consumption.geo.sd <- apply(current.consumption.maps.land, 1, FUN = function(.) exp(sd(log(.))))
current.consumption.025 <- apply(current.consumption.maps.land, 1, quantile, probs = .025)
current.consumption.975 <- apply(current.consumption.maps.land, 1, quantile, probs = .975)

temp.map <- base.map

temp.map[land.i] <- current.consumption.mean
current.consumption.mean.map <- temp.map

temp.map[land.i] <- current.consumption.geo.mean
current.consumption.geo.mean.map <- temp.map

temp.map[land.i] <- current.consumption.geo.sd
current.consumption.geo.sd.map <- temp.map

temp.map[land.i] <- current.consumption.025
current.consumption.025.map <- temp.map

temp.map[land.i] <- current.consumption.975
current.consumption.975.map <- temp.map
toc()


# Calculate present.natural summary statistics
tic()
present.natural.consumption.mean <- apply(present.natural.consumption.maps.land, 1, mean)
present.natural.consumption.geo.mean <- apply(present.natural.consumption.maps.land, 1, FUN = function(.) exp(mean(log(.))))
present.natural.consumption.geo.sd <- apply(present.natural.consumption.maps.land, 1, FUN = function(.) exp(sd(log(.))))
present.natural.consumption.025 <- apply(present.natural.consumption.maps.land, 1, quantile, probs = .025)
present.natural.consumption.975 <- apply(present.natural.consumption.maps.land, 1, quantile, probs = .975)

temp.map <- base.map

temp.map[land.i] <- present.natural.consumption.mean
present.natural.consumption.mean.map <- temp.map

temp.map[land.i] <- present.natural.consumption.geo.mean
present.natural.consumption.geo.mean.map <- temp.map

temp.map[land.i] <- present.natural.consumption.geo.sd
present.natural.consumption.geo.sd.map <- temp.map

temp.map[land.i] <- present.natural.consumption.025
present.natural.consumption.025.map <- temp.map

temp.map[land.i] <- present.natural.consumption.975
present.natural.consumption.975.map <- temp.map
toc()



# Load world map overlay and set up plotting theme ------------------------

# Load world shape overlay
# world.map <- ne_countries(scale = 110, returnclass = "sf") %>% 
#   st_transform(st_crs(base.map)) %>% 
#   st_union()

world.map <- giscoR::gisco_get_countries() %>%
  st_union() %>% 
  st_transform(st_crs(base.map))

# Create ggplot theme
theme_R <- function() {
  theme_bw() %+replace% 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
}



# Load Modis data of NPP --------------------------------------------------

# Modis data are based on leaf area and solar energy input
# It is total NPP, and before _any_ consumption 
# (other than what has been subtracted for respiration)
npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif") # [gC / m2 / yr]
# Cut extremely variable values (>95-quantile SD)
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA




# Cut off low NPP based on WWF Deserts and Xeric shrub ---------------------

# Load WWF biomes
wwf.biome <- base.map
wwf.biome[] <- raster("data/wwf_terr_ecos_biome_raster.tif")[]
# WWF Biome 13: Deserts and Xeric Shrublands
wwf.biome[] <- wwf.biome[] == 13

# NPP summary for Deserts and Xeric Shrublands
npp[wwf.biome == 1] %>% summary

# Cut off at 3rd quantile "most" ~ 200 gC/m2/yr
cut <- 200

# Show what gets cut which is and isn't desert
ggplot(tibble(NPP = npp[], Desert = wwf.biome[] == 1), aes(NPP, col = Desert)) + 
  geom_density() +
  geom_vline(data = NULL, xintercept = cut) +
  theme_R()

# Map what gets cut
plot(npp < cut)

###### Cut or not ######
# full = TRUE # Plot all
full = FALSE # Plot 200 cut
if(full) {
  remove.areas <- NA # For full!
  # Still change NPP cells with 0 to NA
  npp[which(npp[] == 0)] <- NA
} else {
  npp[npp[] < cut] <- NA
  remove.areas <- which(is.na(npp[]))  
}

# Change NPP units to [Mg Carbon / km2 / yr]
npp[] <- npp[] * 1000^2 / 10^6 # Mg Carbon / km2 / yr

# Remove ranges:
current.consumption.geo.mean.map[remove.areas] <- NA
present.natural.consumption.geo.mean.map[remove.areas] <- NA
# current.megafauna.consumption.map[remove.areas] <- NA
# present.natural.megafauna.consumption.map[remove.areas] <- NA

# current.mass.map[remove.areas] <- NA
# present.natural.mass.map[remove.areas] <- NA


# Prepare datasets for plotting ----------------------------------

# Calculate change between CU and PN
change <- (current.consumption.geo.mean.map/present.natural.consumption.geo.mean.map - 1) * 100
# Truncate positive change
change[change > 0] <- 0
change[present.natural.consumption.map == 0] <- 0

# NPP consumption (Carbon consumed / Carbon produced) [%]
# Find the fraction
current.npp.use <- (current.consumption.geo.mean.map)/npp * 100
present.natural.npp.use <- (present.natural.consumption.geo.mean.map)/npp * 100

current.megafauna.npp.use <- (current.megafauna.consumption.map)/npp * 100
present.natural.megafauna.npp.use <- (present.natural.megafauna.consumption.map)/npp * 100

# Truncate fractions above 100 %
current.npp.use[current.npp.use[] >= 100.5] <- 101
present.natural.npp.use[present.natural.npp.use[] >= 100.5] <- 101

current.megafauna.npp.use[current.megafauna.npp.use[] >= 100.5] <- 101
present.natural.megafauna.npp.use[present.natural.megafauna.npp.use[] >= 100.5] <- 101

# Calculate pct-point change in NPP use
change.pct.point <- current.npp.use - present.natural.npp.use
change.pct.point[] <- ifelse(current.npp.use[] == 101 | present.natural.npp.use[] == 101 , 101, change.pct.point[])

change.pct.point2 <- (current.npp.use/present.natural.npp.use - 1) * 100
change.pct.point2[] <- ifelse(current.npp.use[] == 101 | present.natural.npp.use[] == 101 , 101, change.pct.point2[])
change.pct.point2[change.pct.point2 > 0] <- 0


# Turn carbon use into data.frames for plotting (all)
# [MgC / km2 / year]

# Current
current.consumption.df <- current.consumption.geo.mean.map %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")

# Present natural
present.natural.consumption.df <- present.natural.consumption.geo.mean.map %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined
consumption.df <- bind_rows(current.consumption.df, 
                            present.natural.consumption.df)

# Percentage difference
change.df <- change %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Difference")


# Current megafauna consumption
current.megafauna.consumption.df <- current.megafauna.consumption.map %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")

# Present natural megafauna consumption
present.natural.megafauna.consumption.df <- present.natural.megafauna.consumption.map %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined megafauna consumption
megafauna.consumption.df <- bind_rows(current.megafauna.consumption.df, 
                                      present.natural.megafauna.consumption.df)



# Current mass
current.mass.df <- current.mass.map %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")
# Present natural mass
present.natural.mass.df <- present.natural.mass.map %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined mass
mass.df <- bind_rows(current.mass.df, 
                     present.natural.mass.df)

# Turn NPP use into data.frames for plotting (all)
# [%]

# Current
current.npp.use.df <- current.npp.use %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")

# Present natural
present.natural.npp.use.df <- present.natural.npp.use %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined
npp.use <- bind_rows(current.npp.use.df, 
                     present.natural.npp.use.df)

# Percentage point difference
change.pct.df <- change.pct.point %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Percentage point difference")

change.pct.df2 <- change.pct.point2 %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Percentage point difference")

# Turn NPP use into data.frames for plotting (megafauna)
# [%]

# Current
current.megafauna.npp.use.df <- current.megafauna.npp.use %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")

# Present natural
present.natural.megafauna.npp.use.df <- present.natural.megafauna.npp.use %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined
megafauna.npp.use <- bind_rows(current.megafauna.npp.use.df, 
                               present.natural.megafauna.npp.use.df)



# Last of the wild --------------------------------------------------------

# Load and force align to basemap of LTW realms
ltw.realm <- base.map
ltw.realm[] <- raster("data/ltw_v2geo_realm.tif")[]
# NAs to 0 and the rest 1
ltw.realm[is.na(ltw.realm[])] <- 0
ltw.realm[which(ltw.realm[] > 0)] <- 1

# Load and force align to basemap of LTW biomes
ltw.biome <- base.map
ltw.biome[] <- raster("data/ltw_v2geo_biome.tif")[]
# NAs to 0 and the rest 1
ltw.biome[ltw.biome[] == 128] <- 0
ltw.biome[which(ltw.biome[] > 0)] <- 1

# Make new LTW raster based on basemap
ltw <- base.map
# Anywhere in either LTW biomes or realm maps
ltw[ltw.realm[] | ltw.biome[]] <- 1

# Turn into data.frame
ltw <- as(ltw, "SpatialPixelsDataFrame")
ltw <- as_tibble(ltw)
colnames(ltw) <- c("value", "x", "y")

# Add NPP-% use to LTW
ltw <- ltw[, -1] %>% 
  left_join(npp.use, by = c("x", "y"))





# Load WWF realms and biomes --------------------------------------

wwf.realm <- base.map
wwf.realm[] <- raster("data/wwf_terr_ecos_realm_raster.tif")[]
wwf.realm.names <- read_csv("data/wwf_realm_names.csv")

wwf.biome <- base.map
wwf.biome[] <- raster("data/wwf_terr_ecos_biome_raster.tif")[]
wwf.biome.names <- read_csv("data/wwf_biome_names.csv")

na.cells <- which(wwf.realm[] == 4 | wwf.biome[] %in% c(98, 99, 128) | is.na(wwf.realm[]) | is.na(wwf.biome[]))

# Load WWF realms
wwf.realm[na.cells] <- NA
wwf.realm <- wwf.realm %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(wwf_realm = .[[1]],
            x, 
            y) %>% 
  left_join(wwf.realm.names, by = "wwf_realm")

# ggplot(wwf.realm, aes(x = x, y = y, fill = name)) +
#   geom_tile() +
#   scale_fill_viridis(name = "WWF Realm", na.value = "pink", discrete = T, option = "D") +
#   theme_map() +
#   geom_sf(data = world.map, inherit.aes = F, col = "black", fill = NA, lwd = .25) +
#   coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)

# Load WWF biomes
wwf.biome[na.cells] <- NA
wwf.biome <- wwf.biome %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(wwf_biome = .[[1]],
            x, 
            y) %>% 
  left_join(wwf.biome.names, by = "wwf_biome")

# ggplot(wwf.biome, aes(x = x, y = y, fill = name)) +
#   geom_tile() +
#   scale_fill_viridis(name = "WWF biome", na.value = "pink", discrete = T, option = "D") +
#   theme_map() +
#   theme(legend.background = element_blank()) +
#   geom_sf(data = world.map, inherit.aes = F, col = "black", fill = NA, lwd = .25) +
#   coord_sf(ylim = range(land.df$y), xlim = range(land.df$x) * 1.02, expand = FALSE)


# Remove madagascar and other excluded regions ----------------------------

# Madagascar cells
madagascar <- c(33345, 33346, 33347, 33348, 33349, 33350, 33351, 33352, 33353, 
                33705, 33706, 33707, 33708, 33709, 33710, 33711, 33712, 33713, 
                34065, 34066, 34067, 34068, 34069, 34070, 34071, 34072, 34073, 
                34425, 34426, 34427, 34428, 34429, 34430, 34431, 34432, 34433,
                34785, 34786, 34787, 34788, 34789, 34790, 34791, 34792, 34793, 
                35145, 35146, 35147, 35148, 35149, 35150, 35151, 35152, 35153,
                35505, 35506, 35507, 35508, 35509, 35510, 35511, 35512, 35513, 
                35865, 35866, 35867, 35868, 35869, 35870, 35871, 35872, 35873,
                36225, 36226, 36227, 36228, 36229, 36230, 36231, 36232, 36233,
                36585, 36586, 36587, 36588, 36589, 36590, 36591, 36592, 36593, 
                36945, 36946, 36947, 36948, 36949, 36950, 36951, 36952, 36953, 
                37305, 37306, 37307, 37308, 37309, 37310, 37311, 37312, 37313, 
                37665, 37666, 37667, 37668, 37669, 37670, 37671, 37672, 37673, 
                38025, 38026, 38027, 38028, 38029, 38030, 38031, 38032, 38033, 
                38385, 38386, 38387, 38388, 38389, 38390, 38391, 38392, 38393, 
                38745, 38746, 38747, 38748, 38749, 38750, 38751, 38752, 38753,
                39105, 39106, 39107, 39108, 39109, 39110, 39111, 39112, 39113, 
                39465, 39466, 39467, 39468, 39469, 39470, 39471, 39472, 39473, 
                39825, 39826, 39827, 39828, 39829, 39830, 39831, 39832, 39833)

na.cells.expanded <- which(wwf.realm[] %in% c(4, 7) | wwf.biome[] %in% c(98, 99, 128) | is.na(wwf.realm[]) | is.na(wwf.biome[]))
madagascar <- c(33345, 33346, 33347, 33348, 33349, 33350, 33351, 33352, 33353, 33705, 33706, 33707, 33708, 33709, 33710, 33711, 33712, 33713, 34065, 34066, 34067, 34068, 34069, 34070, 34071, 34072, 34073, 34425, 34426, 34427, 34428, 34429, 34430, 34431, 34432, 34433, 34785, 34786, 34787, 34788, 34789, 34790, 34791, 34792, 34793, 35145, 35146, 35147, 35148, 35149, 35150, 35151, 35152, 35153, 35505, 35506, 35507, 35508, 35509, 35510, 35511, 35512, 35513, 35865, 35866, 35867, 35868, 35869, 35870, 35871, 35872, 35873, 36225, 36226, 36227, 36228, 36229, 36230, 36231, 36232, 36233, 36585, 36586, 36587, 36588, 36589, 36590, 36591, 36592, 36593, 36945, 36946, 36947, 36948, 36949, 36950, 36951, 36952, 36953, 37305, 37306, 37307, 37308, 37309, 37310, 37311, 37312, 37313, 37665, 37666, 37667, 37668, 37669, 37670, 37671, 37672, 37673, 38025, 38026, 38027, 38028, 38029, 38030, 38031, 38032, 38033, 38385, 38386, 38387, 38388, 38389, 38390, 38391, 38392, 38393, 38745, 38746, 38747, 38748, 38749, 38750, 38751, 38752, 38753, 39105, 39106, 39107, 39108, 39109, 39110, 39111, 39112, 39113, 39465, 39466, 39467, 39468, 39469, 39470, 39471, 39472, 39473, 39825, 39826, 39827, 39828, 39829, 39830, 39831, 39832, 39833)
na.cells.expanded <- c(na.cells.expanded, madagascar)
na.cells.expanded <- c(na.cells.expanded, remove.areas)
na.cells.expanded <- unique(na.cells.expanded)

wwf.realm <- base.map
wwf.realm[] <- raster("data/wwf_terr_ecos_realm_raster.tif")[]
wwf.realm.names <- read_csv("data/wwf_realm_names.csv")

wwf.biome <- base.map
wwf.biome[] <- raster("data/wwf_terr_ecos_biome_raster.tif")[]
wwf.biome.names <- read_csv("data/wwf_biome_names.csv")

wwf.realm[na.cells.expanded] <- NA
wwf.realm <- wwf.realm %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(wwf_realm = .[[1]],
            x, 
            y) %>% 
  left_join(wwf.realm.names, by = "wwf_realm") %>% 
  transmute(x, y, realm = name)

wwf.biome[na.cells.expanded] <- NA
wwf.biome <- wwf.biome %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(wwf_biome = .[[1]],
            x, 
            y) %>% 
  left_join(wwf.biome.names, by = "wwf_biome") %>% 
  transmute(x, y, biome = name)

eco.units <- left_join(wwf.realm, wwf.biome, by = c("x", "y"))


# Select the system we keep and look at -----------------------------------

# Remove ecosystems we are not interested in
leave.out <- c("Tropical and Subtropical Coniferous Forests", "Flooded Grasslands and Savannas",
               "Montane Grasslands and Shrublands", "Tundra", "Deserts and Xeric Shrublands", "Mangroves")
eco.units <- eco.units %>%
  filter(!biome %in% leave.out)

# Combine connected realms and biomes
eco.units <- eco.units %>% 
  mutate(realm = ifelse(realm %in% c("Nearctic", "Neotropic"), "Americas", realm)) %>% 
  mutate(biome = ifelse(biome %in% c("Temperate Broadleaf and Mixed Forests", "Temperate Coniferous Forests"), "Temperate Forests", biome)) %>% 
  mutate(biome = ifelse(biome %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and subtropical grasslands, savannas, and shrublands"), "Tropical and Subtropical Grasslands to Forests", biome)) %>% 
  mutate(eco.unit = paste(realm, biome, sep = " - "))

# See which regions are left
eco.units %>% count(eco.unit) %>% print(n = 100)

# Filter to an intersting subset with most cells
keep.units <- c(
  "Afrotropic - Tropical and Subtropical Grasslands to Forests",
  "Afrotropic - Tropical and Subtropical Moist Broadleaf Forests",
  "Australasia - Mediterranean Forests, Woodlands, and Scrub",
  "Australasia - Temperate Forests",
  "Australasia - Temperate Grasslands, Savannas, and Shrublands",
  "Australasia - Tropical and Subtropical Grasslands to Forests",
  "Australasia - Tropical and Subtropical Moist Broadleaf Forests",
  "Indomalaya - Tropical and Subtropical Grasslands to Forests",
  "Indomalaya - Tropical and Subtropical Moist Broadleaf Forests",
  "Americas - Boreal Forests/Taiga",
  "Americas - Temperate Forests",
  "Americas - Temperate Grasslands, Savannas, and Shrublands",
  "Americas - Tropical and Subtropical Grasslands to Forests",
  "Americas - Tropical and Subtropical Moist Broadleaf Forests",
  "Palearctic - Boreal Forests/Taiga",
  "Palearctic - Mediterranean Forests, Woodlands, and Scrub",
  "Palearctic - Temperate Forests",
  "Palearctic - Temperate Grasslands, Savannas, and Shrublands")

eco.units <- eco.units %>% filter(eco.unit %in% keep.units)
eco.units %>% count(eco.unit)

# Shorten biome names
eco.units$biome <- str_replace(eco.units$biome, "Temperate Grasslands, Savannas, and Shrublands", "Temperate Grasslands to Shrublands")
eco.units$biome <- str_replace(eco.units$biome, "Mediterranean Forests, Woodlands, and Scrub", "Mediterranean Forests to Scrub")

# Plot Supplementary figure of ecoregions
cols <- colorspace::qualitative_hcl(palette = "Dark 3", n = 5)
breaks = c("Americas", "Afrotropic", "Palearctic", "Indomalaya", "Australasia")
eco.units.plot <- ggplot(eco.units, aes(x = x, y = y, fill = realm)) +
  facet_wrap(vars(biome), nrow = 3) +
  geom_tile() +
  scale_fill_manual(name = "Realm", na.value = "pink", values = cols, breaks = breaks) +
  theme_map() +
  theme(legend.position = "bottom", legend.justification = NULL)  +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = NA, lwd = .25) +
  coord_sf(ylim = range(land.df$y) * 1.02, xlim = range(land.df$x) * 1.02, expand = FALSE)
ggsave("./output/fig_ecoregions.png", eco.units.plot, width = 183, height = 137, units = "mm", dpi = 600)


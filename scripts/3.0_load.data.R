# Load data and packages used in models and graphing
# 27/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)
library(tictoc)
library(Matrix)
library(raster)
library(sf)
library(rnaturalearth)
# library(data.table)
# library(viridis)  # better colors for everyone
# library(cowplot) # Load cowplot (has to be loaded before ggthemes!)
# library(ggthemes)
# library(gridExtra)
# library(rworldmap) # For a worldmap shape overlay
# library(maptools) # unionSpatialPolygons()



# Load data ---------------------------------------------------------------

# Load traits
df <- read_csv("builds/data.csv", col_types = cols())

# Load consumption data
consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]

# Summarise consumption
consumption.summary <- consumption.samples %>%
  transmute(Binomial.1.2,
            mean = rowMeans(.[, -1]),
            median = apply(.[, -1], 1, median),
            ci.lw = apply(.[, -1], 1, quantile, probs = .025),
            ci.hi = apply(.[, -1], 1, quantile, probs = .975))
# Alignment sanity check 
stopifnot(all(consumption.summary$Binomial.1.2 == df$Binomial.1.2))

# Estimate this in terms of plant consumtion
consumption <- consumption.summary %>% 
  mutate(Q.plant = median * df$Diet.Plant/100,
         ci.lw.plant = ci.lw * df$Diet.Plant/100,
         ci.hi.plant = ci.hi * df$Diet.Plant/100)




# Load species maps and calculate consumption maps ------------------------

# Current maps
current.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")
current.consumption <- current.maps * consumption$Q.plant
current.consumption.lw <- current.maps * consumption$ci.lw.plant
current.consumption.hi <- current.maps * consumption$ci.hi.plant

# Present natural maps
present.natural.maps <- read_rds("builds/present.natural.maps.filtered.edge.lim.rds")
present.natural.consumption <- present.natural.maps * consumption$Q.plant
present.natural.consumption.lw <- present.natural.maps * consumption$ci.lw.plant
present.natural.consumption.hi <- present.natural.maps * consumption$ci.hi.plant

# Base map
base.map <- raster("builds/base_map.tif")

# Set up maps to use
current.consumption.map <- base.map
present.natural.consumption.map <- base.map
current.consumption.map.lw <- base.map
present.natural.consumption.map.lw <- base.map
current.consumption.map.hi <- base.map
present.natural.consumption.map.hi <- base.map



# Calculate total consumption per grid cell -------------------------------
# Summarize consumption per grid-cell
# Transform change from [KgC / (km2 * year)] to [MgC / (km2 * year)]

# Current maps
current.consumption.map[] <- colSums(current.consumption) / 10^3
current.consumption.map[current.consumption.map == 0] <- NA

# Current maps CI
current.consumption.map.lw[] <- colSums(current.consumption.lw) / 10^3
current.consumption.map.lw[current.consumption.map.lw == 0] <- NA
current.consumption.map.hi[] <- colSums(current.consumption.hi) / 10^3
current.consumption.map.hi[current.consumption.map.hi == 0] <- NA

# Current megafauna maps
megafauna <- which(df$Mass.g >= 45000)
current.megafauna.consumption.map <- base.map
current.megafauna.consumption.map[] <- colSums(current.consumption[megafauna, ]) / 10^3
current.megafauna.consumption.map[current.megafauna.consumption.map == 0] <- NA


# Present natural maps
present.natural.consumption.map[] <- colSums(present.natural.consumption) / 10^3
present.natural.consumption.map[present.natural.consumption.map == 0] <- NA

# Present natural maps CI
present.natural.consumption.map.lw[] <- colSums(present.natural.consumption.lw) / 10^3
present.natural.consumption.map.lw[present.natural.consumption.map.lw == 0] <- NA
present.natural.consumption.map.hi[] <- colSums(present.natural.consumption.hi) / 10^3
present.natural.consumption.map.hi[present.natural.consumption.map.hi == 0] <- NA

# Present natural megafauna maps
present.natural.megafauna.consumption.map <- base.map
present.natural.megafauna.consumption.map[] <- colSums(present.natural.consumption[megafauna, ]) / 10^3
present.natural.megafauna.consumption.map[present.natural.megafauna.consumption.map == 0] <- NA



# Load world map overlay and set up plotting theme ------------------------

# Load world shape overlay
world.map <- ne_countries(returnclass = "sf") %>% 
  st_transform(st_crs(base.map)) %>% 
  st_union()

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
current.consumption.map[remove.areas] <- NA
present.natural.consumption.map[remove.areas] <- NA
current.megafauna.consumption.map[remove.areas] <- NA
present.natural.megafauna.consumption.map[remove.areas] <- NA



# Prepare datasets for plotting ----------------------------------

# Calculate change between CU and PN
change <- (current.consumption.map/present.natural.consumption.map - 1) * 100
# Truncate positive change
change[change > 0] <- 0

# NPP consumption (Carbon consumed / Carbon produced) [%]
# Find the fraction
current.npp.use <- (current.consumption.map)/npp * 100
present.natural.npp.use <- (present.natural.consumption.map)/npp * 100

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


# Turn carbon use into data.frames for plotting (all)
# [MgC / km2 / year]

# Current
current.consumption.df <- current.consumption.map %>%
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Current")

# Present natural
present.natural.consumption.df <- present.natural.consumption.map %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_tibble() %>% 
  transmute(value = .[[1]],
            x, 
            y, 
            period = "Present natural")

# Combined
consumption.df <- bind_rows(current.consumption.df, 
                            present.natural.consumption.df)


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




# Calculate global consumption summaries ----------------------------------

# Summarise NPP use (all) [%]
(npp.use.summary <- npp.use %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n()))
# PN use is x times higher than CU use
npp.use.summary[[4]][2]/npp.use.summary[[4]][1]
# Which is x % reduction
signif(diff(npp.use.summary[[4]])/npp.use.summary[[4]][2] * 100, 2)

# Summarise NPP use (megafauna) [%]
(megafauna.npp.use.summary <- megafauna.npp.use %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n()))

# Megafauna NPP use compared to all fauna:
# Current
# all
npp.use.summary$mean[1]
# percentage used by megafauna
signif(megafauna.npp.use.summary$mean[1]/npp.use.summary$mean[1] * 100, 2)
# Present natural
# all
npp.use.summary$mean[2]
# percentage used by megafauna
signif(megafauna.npp.use.summary$mean[2]/npp.use.summary$mean[2] * 100, 2)

# Magafauna NPP use drop [%]:
overall.drop <- diff(npp.use.summary$mean)
megafauna.drop <- diff(megafauna.npp.use.summary$mean)
signif(megafauna.drop/overall.drop * 100, 2)

# Summarise NPP use (LTW) [%]
ltw %>% 
  group_by(period) %>% 
  filter(!is.na(value)) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n())

# Summarise carbon consumption (all) [MgC / km2 / year]
consumption.df %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n())




# Calculate total global consumption --------------------------------------
# Units:  
# [MgC / (km2 * year) * m2 * km2 / 10^6 m2 * 10^6 g / Mg * Pg / 10^15 g] = [PgC / year]
tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
diff <- tot.pres.nat - tot.current

tot.current.med <- median(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.med <- median(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.current.mean <- mean(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.mean <- mean(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^6 / 10^15

tot.current.lw <- sum(current.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.lw <- sum(present.natural.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^6 / 10^15
diff.lw <- tot.pres.nat.lw - tot.current.lw

tot.current.hi <- sum(current.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
tot.pres.nat.hi <- sum(present.natural.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^6 / 10^15
diff.hi <- tot.pres.nat.hi - tot.current.hi

paste0("Current consumption: ", signif(tot.current, 2), " Pg Carbon / year (95%-CI: ", signif(tot.current.lw, 2), "-", signif(tot.current.hi, 2) ,")")
paste0("Present natural consumption: ", signif(tot.pres.nat, 2), " Pg Carbon / year (95%-CI: ", signif(tot.pres.nat.lw, 2), "-", signif(tot.pres.nat.hi, 2) ,")")
paste0("Difference: ", signif(diff, 2), " Pg Carbon / year (95%-CI: ", signif(diff.lw, 2), "-", signif(diff.hi, 2) ,")")


# As fraction on NPP [%]

# Total NPP [Pg]
tot.npp <- sum(npp[] * prod(res(npp))/10^6, na.rm = T)*10^6/10^15
tot.npp
# Percentage released (Doughty says 2.2-5.3)
(tot.pres.nat - tot.current)/tot.npp * 100
(tot.pres.nat.lw - tot.current.lw)/tot.npp * 100
(tot.pres.nat.hi - tot.current.hi)/tot.npp * 100

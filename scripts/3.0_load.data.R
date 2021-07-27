# Load libs for section 3.x
library(tidyverse)
library(tictoc)
# library(data.table)
# library(raster)
# library(viridis)  # better colors for everyone
# library(cowplot) # Load cowplot (has to be loaded before ggthemes!)
# library(ggthemes)
# library(gridExtra)
# library(rworldmap) # For a worldmap shape overlay
# library(maptools) # unionSpatialPolygons()


# Load data
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
consumption <- consumption.summary %>% mutate(Q.plant = median * df$Diet.Plant/100,
                                              ci.lw.plant = ci.lw * df$Diet.Plant/100,
                                              ci.hi.plant = ci.hi * df$Diet.Plant/100)


# Load map data and builds consumption maps
#load("builds/current.maps.filtered.RData")
current.maps <- readRDS("builds/current.maps.filtered.edge.lim.rds")
current.consumption <- current.maps * consumption$Q.plant
current.consumption.lw <- current.maps * consumption$ci.lw.plant
current.consumption.hi <- current.maps * consumption$ci.hi.plant
rm(current.maps) # Each loaded matrix is 1.9 GB ram
gc() # R forgets to clean up

present.natural.maps <- readRDS("builds/present.natural.maps.filtered.edge.lim.rds")
present.natural.consumption <- present.natural.maps * consumption$Q.plant
present.natural.consumption.lw <- present.natural.maps * consumption$ci.lw.plant
present.natural.consumption.hi <- present.natural.maps * consumption$ci.hi.plant
rm(present.natural.maps) # Each loaded matrix is 1.9 GB ram
gc() # R forgets to clean up

base.map <- raster("builds/base_map.tif")

current.consumption.map <- base.map
present.natural.consumption.map <- base.map
current.consumption.map.lw <- base.map
present.natural.consumption.map.lw <- base.map
current.consumption.map.hi <- base.map
present.natural.consumption.map.hi <- base.map

# Summarize consumption per grid-cell
# Transform change from [KgC / (km2 * year)] to [MgC / (km2 * year)]
tic()
current.consumption.map[] <- colSums(current.consumption) / 10^3
current.consumption.map[current.consumption.map == 0] <- NA

megafauna <- which(df$Mass.g >= 45000)
current.megafauna.consumption.map <- base.map
current.megafauna.consumption.map[] <- colSums(current.consumption[megafauna, ]) / 10^3
current.megafauna.consumption.map[current.megafauna.consumption.map == 0] <- NA

current.consumption.map.lw[] <- colSums(current.consumption.lw) / 10^3
current.consumption.map.lw[current.consumption.map.lw == 0] <- NA
current.consumption.map.hi[] <- colSums(current.consumption.hi) / 10^3
current.consumption.map.hi[current.consumption.map.hi == 0] <- NA

rm(current.consumption.lw)
rm(current.consumption.hi)
toc()

tic()
present.natural.consumption.map[] <- colSums(present.natural.consumption) / 10^3
present.natural.consumption.map[present.natural.consumption.map == 0] <- NA

megafauna <- which(df$Mass.g >= 45000)
present.natural.megafauna.consumption.map <- base.map
present.natural.megafauna.consumption.map[] <- colSums(present.natural.consumption[megafauna, ]) / 10^3
present.natural.megafauna.consumption.map[present.natural.megafauna.consumption.map == 0] <- NA

present.natural.consumption.map.lw[] <- colSums(present.natural.consumption.lw) / 10^3
present.natural.consumption.map.lw[present.natural.consumption.map.lw == 0] <- NA
present.natural.consumption.map.hi[] <- colSums(present.natural.consumption.hi) / 10^3
present.natural.consumption.map.hi[present.natural.consumption.map.hi == 0] <- NA

rm(present.natural.consumption.lw)
rm(present.natural.consumption.hi)
toc()

gc()


## Load basemap
base.map <- raster("builds/base_map.tif")
## Load world shape overlay
world.map <- getMap(resolution = "low")
world.map <- unionSpatialPolygons(world.map, rep(1, nrow(world.map)))
world.map <- spTransform(world.map, crs(base.map))
world.map <- fortify(world.map)

# Create ggplot theme
theme_R <- function() {
  theme_bw() %+replace% 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
}

## Load Modis data of NPP
# Modis data are based on leaf area and solar energy input
# It is total NPP, and before _any_ consumption 
# (other than what has been subtracted for respiration)
npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif") # [gC / m2 / yr]
# Cut extremely variable values (>95-quantile SD)
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA

## Cut of low NPP based on WWF Deserts and Xeric shrub
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
current.consumption.df <- as(current.consumption.map, "SpatialPixelsDataFrame") # [MgC / km2 / year]
current.consumption.df <- as_tibble(current.consumption.df)
colnames(current.consumption.df) <- c("value", "x", "y")
current.consumption.df <- current.consumption.df %>% mutate(period = "Current")

present.natural.consumption.df <- as(present.natural.consumption.map, "SpatialPixelsDataFrame")
present.natural.consumption.df <- as_tibble(present.natural.consumption.df)
colnames(present.natural.consumption.df) <- c("value", "x", "y")
present.natural.consumption.df <- present.natural.consumption.df %>% mutate(period = "Present natural")

consumption.df <- bind_rows(current.consumption.df, present.natural.consumption.df)

# Turn NPP use into data.frames for plotting (all)
current.npp.use.df <- as(current.npp.use, "SpatialPixelsDataFrame")
current.npp.use.df <- as_tibble(current.npp.use.df)
colnames(current.npp.use.df) <- c("value", "x", "y")

present.natural.npp.use.df <- as(present.natural.npp.use, "SpatialPixelsDataFrame")
present.natural.npp.use.df <- as_tibble(present.natural.npp.use.df)
colnames(present.natural.npp.use.df) <- c("value", "x", "y")

# Combine data.sets (all)
pn.npp.use <- present.natural.npp.use.df %>% mutate(period = "Present natural")
cu.npp.use <- current.npp.use.df %>% mutate(period = "Current")
npp.use <- bind_rows(pn.npp.use, cu.npp.use)
npp.use$period <- as.factor(npp.use$period)
npp.use$period <- fct_relevel(npp.use$period, "Present natural")

# Turn NPP use into data.frames for plotting (megafauna)
current.megafauna.npp.use.df <- as(current.megafauna.npp.use, "SpatialPixelsDataFrame")
current.megafauna.npp.use.df <- as_tibble(current.megafauna.npp.use.df)
colnames(current.megafauna.npp.use.df) <- c("value", "x", "y")

present.natural.megafauna.npp.use.spdf <- as(present.natural.megafauna.npp.use, "SpatialPixelsDataFrame")
present.natural.megafauna.npp.use.df <- as_tibble(present.natural.megafauna.npp.use.spdf)
colnames(present.natural.megafauna.npp.use.df) <- c("value", "x", "y")

# Combine data.sets (megafauna)
pn.megafauna.npp.use <- present.natural.megafauna.npp.use.df %>% mutate(period = "Present natural")
cu.megafauna.npp.use <- current.megafauna.npp.use.df %>% mutate(period = "Current")
megafauna.npp.use <- bind_rows(pn.megafauna.npp.use, cu.megafauna.npp.use)
megafauna.npp.use$period <- as.factor(megafauna.npp.use$period)
megafauna.npp.use$period <- fct_relevel(megafauna.npp.use$period, "Present natural")

## Last of the wild:
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


# Summarise NPP use (all)
(npp.use.sumamry <- npp.use %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n()))

# Summarise NPP use (megafauna)
(megafauna.npp.use.summary <- megafauna.npp.use %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n()))

# Megafauna NPP use percentage:
signif(megafauna.npp.use.summary$mean[1]/npp.use.sumamry$mean[1] * 100, 2)
signif(megafauna.npp.use.summary$mean[2]/npp.use.sumamry$mean[2] * 100, 2)

# Magafauna NPP use drop percentage:
overall.drop <- diff(npp.use.sumamry$mean)
megafauna.drop <- diff(megafauna.npp.use.summary$mean)
signif(megafauna.drop/overall.drop * 100, 2)

# Summarise NPP use (LTW)
ltw %>% 
  group_by(period) %>% 
  filter(!is.na(value)) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n())

# Summarise carbon consumption (all)
consumption.df %>% 
  group_by(period) %>%
  summarise(mean = mean(value, na.rm = T) %>% signif(2),
            sd = sd(value, na.rm = T) %>% signif(2),
            median = median(value, na.rm= T) %>% signif(2),
            q.025 = quantile(value, .025, na.rm = T) %>% signif(2),
            q.975 = quantile(value, .975, na.rm = T) %>% signif(2),
            n = n())

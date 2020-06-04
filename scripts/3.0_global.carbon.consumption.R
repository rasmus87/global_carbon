library(tidyverse)
library(tictoc)
library(data.table)
library(raster)
library(viridis)  # better colors for everyone
library(ggthemes)
library(gridExtra)


# Load data
df <- read_csv("builds/data.csv", col_types = cols())

consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]
consumption.corrected <- read_csv("builds/species.consumption.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]
consumption.naive <- read_csv("builds/species.consumption.kgC.yr.km2.naive.csv", col_types = cols()) # [kgC / (km2 * year)]

all(consumption$Binomial.1.2 == df$Binomial.1.2)
consumption.corrected <- consumption.corrected %>% mutate(Q.plant = Q * df$Diet.Plant/100)

consumption.summary <- consumption.samples %>% 
  transmute(Binomial.1.2, 
            mean = rowMeans(.[, -1]),
            median = apply(.[, -1], 1, median),
            ci.lw = apply(.[, -1], 1, quantile, probs = .025),
            ci.hi = apply(.[, -1], 1, quantile, probs = .975))

ggplot(tibble(), aes(x = log10(Q))) +
  geom_density(data = consumption.naive, col = "yellow") +
  geom_density(data = consumption.corrected, col = "magenta") +
  geom_density(data = consumption.summary, aes(x = log10(mean)), col = "red") +
  geom_density(data = consumption.summary, aes(x = log10(median)), col = "darkgreen") +
  geom_density(data = consumption.summary, aes(x = log10(ci.lw)), col = "darkgreen", lty = 2) +
  geom_density(data = consumption.summary, aes(x = log10(ci.hi)), col = "darkgreen", lty = 2) +
  theme_bw()

all(consumption.summary$Binomial.1.2 == df$Binomial.1.2)
consumption <- consumption.summary %>% mutate(Q.plant = median * df$Diet.Plant/100,
                                              ci.lw.plant = ci.lw * df$Diet.Plant/100,
                                              ci.hi.plant = ci.hi * df$Diet.Plant/100)


# Load map data and builds consumption maps (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
current.consumption <- current.maps * consumption$Q.plant
current.consumption.lw <- current.maps * consumption$ci.lw.plant
current.consumption.hi <- current.maps * consumption$ci.hi.plant
rm(current.maps) # Each loaded matrix is 1.7 GB ram
gc() # R forgets to clean up

load("builds/present.natural.maps.filtered.RData")
present.natural.consumption <- present.natural.maps * consumption$Q.plant
present.natural.consumption.lw <- present.natural.maps * consumption$ci.lw.plant
present.natural.consumption.hi <- present.natural.maps * consumption$ci.hi.plant
rm(present.natural.maps) # Each loaded matrix is 1.7 GB ram
gc() # R forgets to clean up

base.map <- raster("builds/base_map.tif")

current.consumption.map <- base.map
present.natural.consumption.map <- base.map
current.consumption.map.lw <- base.map
present.natural.consumption.map.lw <- base.map
current.consumption.map.hi <- base.map
present.natural.consumption.map.hi <- base.map

tic()
current.consumption.map[] <- colSums(current.consumption)
current.consumption.map[current.consumption.map == 0] <- NA

current.consumption.map.lw[] <- colSums(current.consumption.lw)
current.consumption.map.lw[current.consumption.map.lw == 0] <- NA
current.consumption.map.hi[] <- colSums(current.consumption.hi)
current.consumption.map.hi[current.consumption.map.hi == 0] <- NA
toc()

tic()
present.natural.consumption.map[] <- colSums(present.natural.consumption)
present.natural.consumption.map[present.natural.consumption.map == 0] <- NA

present.natural.consumption.map.lw[] <- colSums(present.natural.consumption.lw)
present.natural.consumption.map.lw[present.natural.consumption.map.lw == 0] <- NA
present.natural.consumption.map.hi[] <- colSums(present.natural.consumption.hi)
present.natural.consumption.map.hi[present.natural.consumption.map.hi == 0] <- NA
toc()
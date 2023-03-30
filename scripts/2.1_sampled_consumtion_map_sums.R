# Estimate consumption per cell per map

# Load libraries
library(tidyverse)
library(tictoc)
library(Matrix)
library(raster)


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
density.samples <- read_csv("builds/sampled.density.distribution.csv", col_types = cols()) # [individuals / km2]

# Alignment sanity check 
stopifnot(all(names(density.samples) == df$Binomial.1.2))

# Transform to biomass [Mg / km2]
mass.samples <- sweep(density.samples, MARGIN = 2, df$Mass.g/1e6, `*`) # [Mg / km2]

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
land.df <- as(land, "SpatialPixelsDataFrame")
land.df <- as_tibble(land.df)
land.df$land <- NA

# cu maps
cu.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")
# Remove oceans
cu.maps.land <- cu.maps[, land.i]
# Remove strict non herbivores
cu.maps.herb.land <- cu.maps.land[-no.herb, ]

# Present natural maps
pn.maps <- read_rds("builds/present.natural.maps.filtered.edge.lim.rds")
# Remove oceans
pn.maps.land <- pn.maps[, land.i]
# Remove strict non herbivores
pn.maps.herb.land <- pn.maps.land[-no.herb, ]


# Calculate total consumption per grid cell per map all--------------------------
timestamp()

# Calculate a current consumption map for every sample for every species
cu.consumption.samples.map <- apply(consumption.samples[1:1000, ], 1, function(.) . * cu.maps.herb.land[, ]) 
cu.consumption.samples.map <- c(cu.consumption.samples.map, apply(consumption.samples[1:1000 + 1000, ], 1, function(.) . * cu.maps.herb.land[, ]))
cu.consumption.samples.map <- c(cu.consumption.samples.map, apply(consumption.samples[1:1000 + 2000, ], 1, function(.) . * cu.maps.herb.land[, ]))

# Sum total current consumption for each sample
cu.consumption.maps.land <- sapply(cu.consumption.samples.map, colSums) / 1000 # [MgC / km2 / year]
rm(cu.consumption.samples.map)
gc()


timestamp()
# Calculate a present natural consumption map for every sample for every species
pn.consumption.samples.map <- apply(consumption.samples[1:1000, ], 1, function(.) . * pn.maps.herb.land[, ]) 
pn.consumption.samples.map <- c(pn.consumption.samples.map, apply(consumption.samples[1:1000 + 1000, ], 1, function(.) . * pn.maps.herb.land[, ]))
pn.consumption.samples.map <- c(pn.consumption.samples.map, apply(consumption.samples[1:1000 + 2000, ], 1, function(.) . * pn.maps.herb.land[, ]))

# Sum total present natural consumption for each sample
pn.consumption.maps.land <- sapply(pn.consumption.samples.map, colSums) / 1000 # [MgC / km2 / year]
rm(pn.consumption.samples.map)
gc()



# Calculate total consumption per grid cell per map [Megafauna only] --------------------------
megafauna <- df$Binomial.1.2[which(df$Mass.g >= 45000)]
megafauna.i <- which(names(consumption.samples) %in% megafauna)

timestamp()
# Calculate a current consumption map for every sample for every species
cu.consumption.samples.megafauna.map <- apply(consumption.samples[, megafauna.i], 1, function(.) . * cu.maps.herb.land[megafauna.i, ]) 

# Sum total current consumption for each sample
cu.consumption.megafauna.maps.land <- sapply(cu.consumption.samples.megafauna.map, colSums) / 1000 # [MgC / km2 / year]
rm(cu.consumption.samples.megafauna.map)
gc()

timestamp()
# Calculate a present natural consumption map for every sample for every species
pn.consumption.samples.megafauna.map <- apply(consumption.samples[, megafauna.i], 1, function(.) . * pn.maps.herb.land[megafauna.i, ]) 

# Sum total present natural consumption for each sample
pn.consumption.megafauna.maps.land <- sapply(pn.consumption.samples.megafauna.map, colSums) / 1000 # [MgC / km2 / year]
rm(pn.consumption.samples.megafauna.map)
gc()


# Calculate total biomass per grid cell per map all-----------------------------
timestamp()

# Calculate a current mass map for every sample for every species
cu.mass.samples.map <- apply(mass.samples[1:1000, ], 1, function(.) . * cu.maps.land[, ]) 
cu.mass.samples.map <- c(cu.mass.samples.map, apply(mass.samples[1:1000 + 1000, ], 1, function(.) . * cu.maps.land[, ]))
cu.mass.samples.map <- c(cu.mass.samples.map, apply(mass.samples[1:1000 + 2000, ], 1, function(.) . * cu.maps.land[, ]))

# Sum total current mass for each sample
cu.mass.maps.land <- sapply(cu.mass.samples.map, colSums) # [Mg / km2]
rm(cu.mass.samples.map)
gc()


timestamp()
# Calculate a present natural mass map for every sample for every species
pn.mass.samples.map <- apply(mass.samples[1:1000, ], 1, function(.) . * pn.maps.land[, ]) 
pn.mass.samples.map <- c(pn.mass.samples.map, apply(mass.samples[1:1000 + 1000, ], 1, function(.) . * pn.maps.land[, ]))
pn.mass.samples.map <- c(pn.mass.samples.map, apply(mass.samples[1:1000 + 2000, ], 1, function(.) . * pn.maps.land[, ]))

# Sum total present natural mass for each sample
pn.mass.maps.land <- sapply(pn.mass.samples.map, colSums) # [Mg / km2]
rm(pn.mass.samples.map)
gc()





# Calculate summary statistics ------------------------
summary.stats.maps <- function(x) {
  mean <- apply(x, 1, mean)
  geo.mean <- apply(x, 1, FUN = function(.) exp(mean(log(.))))
  geo.sd <- apply(x, 1, FUN = function(.) exp(sd(log(.))))
  q025 <- apply(x, 1, quantile, probs = .025)
  q975 <- apply(x, 1, quantile, probs = .975)
  
  res <- list(mean = base.map,
              geo.mean = base.map, 
              geo.sd = base.map, 
              q025 = base.map, 
              q975 = base.map)
  
  res$mean[land.i] <- mean
  res$geo.mean[land.i] <- geo.mean
  res$geo.sd[land.i] <- geo.sd
  res$q025[land.i] <- q025
  res$q975[land.i] <- q975
  
  return(res)
}


cu.consumption.maps <- summary.stats.maps(cu.consumption.maps.land)
pn.consumption.maps <- summary.stats.maps(pn.consumption.maps.land)

cu.megafauna.consumption.maps <- summary.stats.maps(cu.consumption.megafauna.maps.land)
pn.megafauna.consumption.maps <- summary.stats.maps(pn.consumption.megafauna.maps.land)

cu.biomass.maps <- summary.stats.maps(cu.mass.maps.land)
pn.biomass.maps <- summary.stats.maps(pn.mass.maps.land)

maps <- as.list(cu.consumption.maps, pn.consumption.maps, 
             cu.megafauna.consumption.maps, pn.megafauna.consumption.maps,
             cu.biomass.maps, pn.biomass.maps)
names(maps) <- c("cu.consumption.maps", "pn.consumption.maps", 
                 "cu.megafauna.consumption.maps", "pn.megafauna.consumption.maps",
                 "cu.biomass.maps", "pn.biomass.maps")

write_rds(maps, "builds/summary.stats.maps.rds")


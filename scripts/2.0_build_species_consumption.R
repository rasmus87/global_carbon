# Estimate consumption per species

# Load libraries
library(tidyverse)
library(tictoc)



# Load data ---------------------------------------------------------------

# Load traits data
df <- read_csv("builds/data.csv", col_types = cols())

# Load FMR posterior distribution
log10fmr.samples <- read_csv("../metabolic_rate/builds/fmr_post.pred.csv") # log10 kJ/day
# Make sure all species are there
all(df$Binomial.1.2 %in% names(log10fmr.samples))
# Subset
log10fmr.samples <- log10fmr.samples[df$Binomial.1.2]
# Make sure alignment is right
stopifnot(all.equal(names(log10fmr.samples), df$Binomial.1.2))


# Load population density posterior distribution
log10dens.samples <- read_csv("../mammal_density/builds/densities_post.pred.csv") # log10 individuals / km2
# Make sure all species are there
all(df$Binomial.1.2 %in% names(log10dens.samples))
# Subset
log10dens.samples <- log10dens.samples[df$Binomial.1.2]
# Make sure alignment is right
stopifnot(all.equal(names(log10dens.samples), df$Binomial.1.2))

# Set number of samples used
stopifnot(nrow(log10fmr.samples) == nrow(log10dens.samples))
n.samples <- nrow(log10fmr.samples)


# Translate FMR to carbon consumption per species -----------------------------

## Metabolizable energy in plant dry matter:
# Degen, A. A., Benjamin, R. W., Abdraimov, S. A., & Sarbasov, T. I. (2002).
# Browse selection by Karakul sheep in relation to plant composition and
# estimated metabolizable energy content. Journal of Agricultural Science,
# 139(3), 353–358. https://doi.org/10.1017/S0021859602002551
# Sheep ME; 8.5 # MJ / kg DM, sd = 1.4
ME.dm = 8.5 * 1000 # kJ / kg DM
ME.dm.sd <- 1.4 * 1000
ME.dm.samples <- matrix(rep(rnorm(n.samples, ME.dm, ME.dm.sd), nrow(df)),
                        byrow = T, nrow = nrow(df))

## Carbon content in dry matter:
# For different parts of the plant: 
# 45.01±5.23, 45.64±4.95, 46.85±3.98,  47.88±3.49
# Ma, S. et al. Variations and determinants of carbon content in plants: A
# global synthesis. Biogeosciences 15, 693–702 (2018).
mean.CC <- mean(45.01, 45.64, 46.85, 47.88)/100 # Carbon content: [kgC / kgDM]
mean.SD.CC <- mean(5.23, 4.95, 3.98,  3.49)/100 # SD

# Metabolizable energy in plant Carbon:
# ..in means
ME.carbon <- ME.dm / mean.CC # [kJ / kg DM] / [kgC /  kgDM] = [kJ / kgC]
# ..in sampled distribution
CC.samples <- matrix(rep(rnorm(n.samples, mean.CC, mean.SD.CC), nrow(df)),
                     byrow = T, nrow = nrow(df))
ME.carbon.samples <- ME.dm.samples / CC.samples # [kJ / kgC]


# Species biomass consumption sampled distribution
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr.samples <- 10^log10fmr.samples * 365.25 / ME.carbon.samples

# Species biomass consumption pr km2 sampled distribution
density.samples <- 10^log10dens.samples # [individuals / km2]
# colnames(density.samples) <- paste0("sample.", 1:n.samples)
# density.samples.table <- bind_cols(Binomial.1.2 = df$Binomial.1.2, as_tibble(density.samples))
write_csv(density.samples, "builds/sampled.density.distribution.csv")

Q.samples = density.samples * biomass.consumption.kgC.yr.samples # [individuals / km2] * [kgC / year]
# colnames(Q.samples) <- paste0("sample.", 1:n.samples)
# consumption.samples <- bind_cols(Binomial.1.2 = df$Binomial.1.2, as_tibble(Q.samples))
write_csv(Q.samples, "builds/sampled.consumption.distribution.kgC.yr.km2.csv")





# Translate FMR to carbon consumption per species using means only --------

# Species biomass consumption pr km2 [[Corrected means]]
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr2 <- df$fmr.mean * 365.25 / ME.carbon
density <- df$density.mean
Q = density * biomass.consumption.kgC.yr # [individuals / km2] * [kgC / year]
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.means.kgC.yr.km2.csv")

# Species biomass consumption pr km2 [[Geometric means]]
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr <- df$fmr.geo.mean * 365.25 / ME.carbon 
density <- df$density.geo.mean
Q = density * biomass.consumption.kgC.yr # [1 / km2] * [kgC / year] = [kgC / (km2 * year)]
consumption.geo <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption.geo, "builds/species.consumption.geo.means.kgC.yr.km2.csv")


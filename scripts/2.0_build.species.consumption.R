# Load libs
library(tidyverse)
library(tictoc)

# Load data
df <- read_csv("builds/data.csv", col_types = cols())
# Load FMR posterior distribution
fmr <- read_csv("../metabolic_rate/builds/3_fmr_post.pred.csv") # kJ/day
stopifnot(all.equal(names(fmr), df$Binomial.1.2))

# Load population density posterior distribution
dens <- read_csv("../mammal_density/builds/3_densities_post.pred.csv") # individuals / km2
stopifnot(all.equal(names(dens), df$Binomial.1.2))

# Set seed to get consistent results:
set.seed(42)

# Sample the FMR and density data
n.samples <- 1000
log10fmr.samples <- t(sample_n(fmr, n.samples))
log10dens.samples <- t(sample_n(dens, n.samples))


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
Q.samples = density.samples * biomass.consumption.kgC.yr.samples # [individuals / km2] * [kgC / year]
colnames(Q.samples) <- paste0("Q.sample.", 1:n.samples)
consumption.samples <- bind_cols(Binomial.1.2 = df$Binomial.1.2, as_tibble(Q.samples))
write_csv(consumption.samples, "builds/sampled.consumption.distribution.kgC.yr.km2.csv")


# Species biomass consumption pr km2
# When data stems from a log-log model we need to correct else we will get
# medians instead of mean
# Smith, R. J. (1993). Logarithmic transformation bias in allometry. American
# Journal of Physical Anthropology, 90(2), 215–228.
# https://doi.org/10.1002/ajpa.1330900208
# Strimbu, B. (2012). Correction for bias of models with lognormal distributed
# variables in absence of original data. Annals of Forest Research, 55(2),
# 265–279.
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr <- exp((log(10) * df$se.fmr)^2/2) * 10^df$log10fmr * 365.25 / ME.carbon # log corrected version
density <- exp((log(10) * df$se.dens)^2/2) * 10^df$log10density # log corrected version
Q = density * biomass.consumption.kgC.yr # [individuals / km2] * [kgC / year]
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.csv")

# Species biomass consumption pr km2 [[Naive version]]
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr <- 10^df$log10fmr * 365.25 / ME.carbon # Naive version
density <- 10^df$log10density # Naive version
Q = density * biomass.consumption.kgC.yr # [1 / km2] * [kgC / year] = [kgC / (km2 * year)]
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.naive.csv")

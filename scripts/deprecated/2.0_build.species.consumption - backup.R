library(tidyverse)
library(tictoc)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
load("builds/present.natural.maps.filtered.RData")

# Load Phylacine trait data
df <- read_csv("builds/df.1.1.csv", col_types = cols())

######## NEW EDITION
# We sample the distributions instead
n.samples <- 1000
tic()
fmr <- read_csv("../metabolic_rate/builds/3_fmr_post.pred.csv")
all.equal(names(fmr), df$Binomial.1.2)
log10fmr.samples <- t(sample_n(fmr, n.samples))
# ... and density distributions
dens <- read_csv("../mammal_density/builds/3_densities_post.pred.csv")
all.equal(names(dens), df$Binomial.1.2)
log10dens.samples <- t(sample_n(dens, n.samples))
toc()
####################

#Degen, A. A., Benjamin, R. W., Abdraimov, S. A., & Sarbasov, T. I. (2002).
#Browse selection by Karakul sheep in relation to plant composition and
#estimated metabolizable energy content. Journal of Agricultural Science,
#139(3), 353–358. https://doi.org/10.1017/S0021859602002551
#Sheep ME; 8.5 # kJ / g , sd = 1.4 # For fucking dry matter you ass-haT!!! >_<


# 17000 kJ/kg carbohydrates, assuming 50 % is accesible: Metabolic energy
ME.carbon = 8500 # kJ / kg C
ME.carbon.sd <- 1400
ME.carbon.samples <- matrix(rep(rnorm(n.samples, ME.carbon, ME.carbon.sd), nrow(df)), byrow=T, nrow=nrow(df))

# ### CORRECTION
# # For different parts of the plant: 
# # 45.01±5.23, 45.64±4.95, 46.85±3.98,  47.88±3.49
# # Ma, S. et al. Variations and determinants of carbon content in plants: A
# # global synthesis. Biogeosciences 15, 693–702 (2018).
# mean_CC <- mean(45.01, 45.64, 46.85, 47.88) # Carbon content (%)
# mean_SD_CC <- mean(5.23, 4.95, 3.98,  3.49) # SD
# C_per_DM <- mean_CC/100 # Carbon content g C / g DM
# ME <- ME / C_per_DM # kJ / (kgC/kgDM)
# C_per_DM.samples <- matrix(rep(rnorm(n.samples, mean_CC, mean_SD_CC), nrow(df)), byrow = T, nrow = nrow(df)) / 100
# ME.samples2 <- ME.samples / C_per_DM.samples # kJ / (kgC/kgDM)
# ##############


### CORRECTION NEW!
# For different parts of the plant: 
# 45.01±5.23, 45.64±4.95, 46.85±3.98,  47.88±3.49
# Ma, S. et al. Variations and determinants of carbon content in plants: A
# global synthesis. Biogeosciences 15, 693–702 (2018).
mean_CC <- mean(45.01, 45.64, 46.85, 47.88)/100 # Carbon content kgC / kgDM
mean_SD_CC <- mean(5.23, 4.95, 3.98,  3.49)/100 # SD
ME.dm <- ME.carbon * mean_CC # (kJ / kgC) * (kgC /  kgDM) = kj / kgDM
CC.samples <- matrix(rep(rnorm(n.samples, mean_CC, mean_SD_CC), nrow(df)), byrow = T, nrow = nrow(df))
ME.dm.samples <- ME.carbon.samples * CC.samples # kJ / kgDM
##############

# Species biomass consumption sampled distribution
biomass.consumption.kgC.yr.samples <- 10^log10fmr.samples * 365.25 / ME.samples

# Species biomass consumption pr km2 sampled distribution
density.samples <- 10^log10dens.samples
Q.samples = density.samples * biomass.consumption.kgC.yr.samples
colnames(Q.samples) <- paste0("Q.sample.", 1:n.samples)
consumption.samples <- bind_cols(Binomial.1.2 = df$Binomial.1.2, as_data_frame(Q.samples))
write_csv(consumption.samples, "builds/sampled.consumption.distribution.kgC.yr.km2.csv")

# Species biomass consumption pr km2
biomass.consumption.kgC.yr <- exp((log(10) * df$se.fmr)^2/2) * 10^df$log10fmr * 365.25 / ME # log corrected version
density <- exp((log(10) * df$se.dens)^2/2) * 10^df$log10density # log corrected version
Q = density * biomass.consumption.kgC.yr
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.csv")

# Species biomass consumption pr km2
biomass.consumption.kgC.yr <- 10^df$log10fmr * 365.25 / ME # Naive version
density <- 10^df$log10density # Naive version
Q = density * biomass.consumption.kgC.yr
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.naive.csv")


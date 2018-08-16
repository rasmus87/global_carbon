library(tidyverse)
library(tictoc)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
load("builds/present.natural.maps.filtered.RData")

# Load Phylacine trait data
df <- read_csv("builds/df.1.1.csv", col_types = cols())

# Sample FMR distribution...
n.samples <- 1000
tic()
log10FMR.samples <- sapply(1:nrow(df), 
                           FUN = function(i) {
                             rnorm(n = n.samples, 
                                   mean = df$log10fmr.est[i], 
                                   sd = df$se.fmr[i])
                           }
)
log10FMR.samples <- t(log10FMR.samples)
# ... and density distributions
log10dens.samples <- sapply(1:nrow(df), 
                            FUN = function(i) {
                              rnorm(n = n.samples, 
                                    mean = df$log10dens.est[i], 
                                    sd = df$se.dens[i])
                            }
)
log10dens.samples <- t(log10dens.samples)
toc()

# 17000 kJ/kg carbohydrates, assuming 50 % is accesible: Metabolic energy
ME = 8500 # kJ / kg
ME.samples <- matrix(rep(rnorm(n.samples, 8.5, 1.4) * 1000, nrow(df)), byrow=T, nrow=nrow(df))

# Species biomass consumption sampled distribution
biomass.consumption.kgC.yr.samples <- 10^log10FMR.samples * 365.25 / ME.samples

# Species biomass consumption pr km2 sampled distribution
density.samples <- 10^log10dens.samples
Q.samples = density.samples * biomass.consumption.kgC.yr.samples
colnames(Q.samples) <- paste0("Q.sample.", 1:n.samples)
consumption.samples <- bind_cols(Binomial.1.2 = df$Binomial.1.2, as_data_frame(Q.samples))
write_csv(consumption.samples, "builds/sampled.consumption.distribution.kgC.yr.km2.csv")

# Species biomass consumption pr km2
biomass.consumption.kgC.yr <- exp((log(10) * df$se.fmr)^2/2) * 10^df$log10fmr.est * 365.25 / ME # log corrected version
#biomass.consumption.kgC.yr <- 10^fmr$log10FMR_est * 365.25 / ME # Naive version
density <- exp((log(10) * df$se.dens)^2/2) * 10^df$log10dens.est # log corrected version
#density <- 10^dens$log10dens.est # Naive version
Q = density * biomass.consumption.kgC.yr
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.csv")


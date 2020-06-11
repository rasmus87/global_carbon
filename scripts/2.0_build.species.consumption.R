library(tidyverse)
library(tictoc)

# Set seed to get consistent results:
set.seed(42)

# Load data
df <- read_csv("builds/data.csv", col_types = cols())

# We sample the distributions instead
n.samples <- 1000
timestamp()
tic()
fmr <- read_csv("../metabolic_rate/builds/3_fmr_post.pred.csv") # kJ/day
all.equal(names(fmr), df$Binomial.1.2)
log10fmr.samples <- t(sample_n(fmr, n.samples))
# ... and density distributions
dens <- read_csv("../mammal_density/builds/3_densities_post.pred.csv") # individuals / km2
all.equal(names(dens), df$Binomial.1.2)
log10dens.samples <- t(sample_n(dens, n.samples))
toc()


# Global mammal biosmass in Carbon >>>
# For comparison with:
# Bar-On, Y. M., Phillips, R., & Milo, R. (2018). The biomass distribution on
# Earth. Proceedings of the National Academy of Sciences of the United States of
# America, 115(25), 6506–6511. https://doi.org/10.1073/pnas.1711842115
# Following their methods dry weight is 30 % and carbon is 50 % 
# so kgC = 0.3 * 0.5 kg wet weight = 0.15 * kg wet weight in total
# [Pg] = [GtC] = 0.15 * BM * 

library(raster)
load("builds/current.maps.filtered.edge.lim.RData")
load("builds/present.natural.maps.filtered.edge.lim.RData")
n.cells.cu <- rowSums(current.maps.edge.lim[])
n.cells.pn <- rowSums(present.natural.maps.edge.lim[])
base.map <- raster("builds/base_map.tif")
# dens [1/km2] * mass [g] * 10^-15 Pg/g * cell-resolution [m^2] * 10^-6 (km/m)^2
# Pg

cell.area <- prod(res(base.map)) * 10^-6 # km^2
wet_to_carbon <- .50 * .30 # carbon.mass / wet.mass
mass.Pg <- df$Mass.g * 10^-15 # Pg (or Gt)
density <- 10^df$log10density # km^-2
mass.pr.cell <- density * mass.Pg * wet_to_carbon * cell.area # Pg Carbon

sum(mass.pr.cell * n.cells.cu)
sum(mass.pr.cell * n.cells.pn)

density.samples <- 10^log10dens.samples # km^-2
mass.pr.cell.samples <- density.samples * mass.Pg * wet_to_carbon * cell.area # Pg Carbon
mass.pr.cell.summary <- apply(mass.pr.cell.samples, 1, function(x) c(quantile(x, 0.025), median = median(x), quantile(x, 0.975)))
mass.cu <- colSums(t(mass.pr.cell.summary) * n.cells.cu)
mass.pn <- colSums(t(mass.pr.cell.summary) * n.cells.pn)

paste0("Current mass: ", signif(mass.cu[2], 2), " PgC (95%-CI: ", signif(mass.cu[1], 2), "-", signif(mass.cu[3], 2), ")")
paste0("Present natural mass: ", signif(mass.pn[2], 2), " PgC (95%-CI: ", signif(mass.pn[1], 2), "-", signif(mass.pn[3], 2), ")")

# The paper says:
# Current: 0.003 Pg (livestock = 0.1)
# Present natural: 0.02 Pg
# Global mammal biosmass in Carbon |||

#Degen, A. A., Benjamin, R. W., Abdraimov, S. A., & Sarbasov, T. I. (2002).
#Browse selection by Karakul sheep in relation to plant composition and
#estimated metabolizable energy content. Journal of Agricultural Science,
#139(3), 353–358. https://doi.org/10.1017/S0021859602002551
#Sheep ME; 8.5 # MJ / kg DM, sd = 1.4
ME.dm = 8.5 * 1000 # kJ / kg DM
ME.dm.sd <- 1.4 * 1000
ME.dm.samples <- matrix(rep(rnorm(n.samples, ME.carbon, ME.carbon.sd), nrow(df)),
                        byrow = T, nrow = nrow(df))

### CORRECTION NEW!
# For different parts of the plant: 
# 45.01±5.23, 45.64±4.95, 46.85±3.98,  47.88±3.49
# Ma, S. et al. Variations and determinants of carbon content in plants: A
# global synthesis. Biogeosciences 15, 693–702 (2018).
mean_CC <- mean(45.01, 45.64, 46.85, 47.88)/100 # Carbon content: [kgC / kgDM]
mean_SD_CC <- mean(5.23, 4.95, 3.98,  3.49)/100 # SD

ME.carbon <- ME.dm / mean_CC # [kJ / kg DM] / [kgC /  kgDM] = [kJ / kgC]

CC.samples <- matrix(rep(rnorm(n.samples, mean_CC, mean_SD_CC), nrow(df)), byrow = T, nrow = nrow(df))
ME.carbon.samples <- ME.dm.samples / CC.samples # [kJ / kgC]
##############

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
# When data stems from a log-log model we need to correc the means
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

# Species biomass consumption pr km2
# [kgC / year] = [kJ/day] * [day/year] / [kJ / kgC]
biomass.consumption.kgC.yr <- 10^df$log10fmr * 365.25 / ME.carbon # Naive version
density <- 10^df$log10density # Naive version
Q = density * biomass.consumption.kgC.yr # [1 / km2] * [kgC / year] = [kgC / (km2 * year)]
consumption <- bind_cols(Binomial.1.2 = df$Binomial.1.2, Q = Q)
write_csv(consumption, "builds/species.consumption.kgC.yr.km2.naive.csv")


# Check differences:
a1 <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv")
a1.med <- apply(a1[, -1], 1, median)
a1.mean <- apply(a1[, -1], 1, mean)
a1Tmed <- tibble(value = a1.med, type = "a1.med")
a1Tmean <- tibble(value = a1.mean, type = "a1.mean")
b1 <- read_csv("builds/species.consumption.kgC.yr.km2.csv")
b1T <- tibble(value = b1$Q, type = "b1")
c1 <- read_csv("builds/species.consumption.kgC.yr.km2.naive.csv")
c1T <- tibble(value = c1$Q, type = "c1")

t1 <- bind_rows(a1Tmed, a1Tmean, b1T, c1T)

ggplot(t1, aes(x = value, col = type)) +
  geom_density()
library(tidyverse)
library(stringr)
library(readxl)

n.samples = 10000

# Load HC datasheet
hc <- read_xlsx("data/halls cave clean.xlsx", sheet = "hall cave clean")
hc <- hc %>% mutate(bin = paste(genus.new, 
                                ifelse(is.na(likely.sp), species.new, likely.sp),
                                sep = "_"))

# Load Phylacine
mam <- read_csv("../PHYLACINE_1.1/Data/Traits/Trait_data.csv", col_types = cols())

# Do all species line up?
hc <- hc %>% rename(Binomial.1.2 = bin)
stopifnot(all(hc$Binomial.1.2 %in% mam$Binomial.1.2))
hc <- hc %>% left_join(mam, "Binomial.1.2")# %>% filter(Diet.Plant > 90)

species <- unique(hc$Binomial.1.2)
# Load FMR and sample distribution
fmr <- read_csv("../metabolic_rate/builds/imputed.metabolic.rate.csv", col_types = cols())
fmr <- fmr[match(species, fmr$Binomial.1.2), ]

df.fmr <- 100
fmr <- fmr %>% mutate(se.fmr = (log10FMR_uprCI - log10FMR_lwrCI)/2/qt(0.975, df.fmr))

log10FMR_samples <- sapply(1:nrow(fmr), 
                           FUN = function(i) {
                             rnorm(n = n.samples, 
                                   mean = fmr$log10FMR_est[i], 
                                   sd = fmr$se.fmr[i])
                           }
                          )
log10FMR_samples <- t(log10FMR_samples)

a <- fmr %>% transmute(Binomial.1.2, test = (log10FMR_est - log10FMR_uprCI)/se.fmr)

# Load animal density and sample distribution
dens <- read_csv("../predict_density/output/animal.density.km2.csv", col_types = cols())
dens <- dens[match(species, dens$Binomial.1.2), ]

log10dens_samples <- sapply(1:nrow(dens), 
                           FUN = function(i) {
                             rnorm(n = n.samples, 
                                   mean = dens$log10dens.est[i], 
                                   sd = dens$se.fit[i])
                           }
)
log10dens_samples <- t(log10dens_samples)


# Calculate metabolic requirements:
# MR = Assimilation.fraction * Food.intake * energy.content <=>
# Food.intake = MR / (Assimilation.fraction * energy.content)
# Energy content (Dry matter):
# Protein and carbohydrates: 4 kcal/g
# Fat: 9 kcal/g
# For assimilation fraction refs see:
# Nagy, K. A. (1987). Field Metabolic Rate and Food Requirement Scaling in
# Mammals and Birds. Source: Ecological Monographs Ecological Monographs,
# 57(572), 111–128.
# Howler monkeys are around 35 %:
# Nagy, K. A., Milton, K., Nagy, K. A., & Milton, K. (2016). Energy Metabolism
# and Food Consumption by Wild Howler Monkeys, 60(3), 475–480.
Assimilation.fraction = 35/100

#Degen, A. A., Benjamin, R. W., Abdraimov, S. A., & Sarbasov, T. I. (2002).
#Browse selection by Karakul sheep in relation to plant composition and
#estimated metabolizable energy content. Journal of Agricultural Science,
#139(3), 353–358. https://doi.org/10.1017/S0021859602002551
#Sheep ME; 8.5 # kJ / g , sd = 1.4

# Food component	Energy density
# http://www.fao.org/docrep/006/Y5022E/y5022e04.htm
#               kJ/g	kcal/g
# Fat	          37 	  9
# Ethanol	      29	  7
# Proteins	    17	  4
# Carbohydrates	17	  4
# Organic acids	13	  3
# Polyols     	10	  2.4
# Fiber       	 8	  2

# Assuming almost pure carb/protein diet.
Energy.content = 17 # kJ/g
Energy.content = Energy.content * 1000 # kJ / kg

ME = (Energy.content * Assimilation.fraction)
# From sheeps diet:
ME = 8500 # kJ / kg
ME.samples <- matrix(rep(rnorm(n.samples, 8.5, 1.4) * 1000, 84), byrow=T, nrow=84)

biomass.consumption.kgC.yr.samples <- 10^log10FMR_samples * 365.25 / ME.samples

density.samples <- 10^log10dens_samples
Q.samples = density.samples * biomass.consumption.kgC.yr.samples
colnames(Q.samples) <- paste0("Q.sample.", 1:n.samples)
consumption.samples <- bind_cols(Binomial.1.2 = species, as_data_frame(Q.samples))

sigma.mr <- 0.2877992
# biomass.consumption.kgC.yr <- 10^(fmr$log10FMR_est + sigma.mr^2/2) * 365.25 / ME # Corrected version
# biomass.consumption.kgC.yr <- 10^(fmr$log10FMR_est + fmr$se.fmr^2/2) * 365.25 / ME # New corrected version
biomass.consumption.kgC.yr <- exp((log(10) * fmr$se.fmr)^2/2) * 10^fmr$log10FMR_est * 365.25 / ME # New-New corrected version
#biomass.consumption.kgC.yr <- 10^fmr$log10FMR_est * 365.25 / ME
sigma.density <- 0.6373866
# density <- 10^(dens$log10dens.est + sigma.density^2/2) # Corrected version
# density <- 10^(dens$log10dens.est + dens$se.fit^2/2) # New corrected version
density <- exp((log(10) * dens$se.fit)^2/2) * 10^dens$log10dens.est # New-New corrected version
#density <- 10^dens$log10dens.est
Q = density * biomass.consumption.kgC.yr
consumption <- bind_cols(Binomial.1.2 = species, Q = Q)

# Gather HC to a long dataset
hc <- hc %>% gather(period, presence, modern:X20752_20988)
hc <- hc %>% filter(presence == 1) %>% select(-presence)
# Remove duplicated species per time period
hc <- hc %>% distinct(Binomial.1.2, period, .keep_all = TRUE)

# Add mean time to each period:
time <- read_csv("data/time.bins.csv", col_types = cols())
hc <- left_join(hc, time, "period")
# Drop unsure timebin
hc <- hc %>% filter(period != "X17216_18159")

hc <- left_join(hc, consumption, by = "Binomial.1.2")

npp.hc <- 322.3 # g Carbon / m2 / yr
npp.hc.kgC_km2_yr <- npp.hc/1000*1000^2 # kgC / km2 / yr

consumption.tot <- hc %>%
  group_by(time) %>%
  summarise(consumption = sum(Q))

hc.sample <- left_join(hc, consumption.samples, by = "Binomial.1.2")
hc.sample <- gather(hc.sample, Q.sample, Q.est, Q.sample.1:paste0("Q.sample.", n.samples))
consumption.tot.sample <- hc.sample %>%
  group_by(time, Q.sample) %>%
  summarise(consumption = sum(Q.est))

consumption.tot.sample %>% group_by(time) %>% summarise(mean(consumption))

source("scripts/coda.hpdinterval.R")

ci95 <- consumption.tot.sample %>%
  group_by(time) %>%
  summarise(q025 = quantile(consumption, 0.025),
            q975 = quantile(consumption, 0.975),
            q25 = quantile(consumption, 0.25),
            q75 = quantile(consumption, 0.75),
            hpd.lwr = hpd.interval(consumption)[1],
            hpd.upr = hpd.interval(consumption)[2],
            median = median(consumption),
            mean = mean(consumption))

change <- ci95 %>% mutate(group = c(rep("after", 9), rep("before", 6)))
change <- change %>% group_by(group) %>% summarise_all(mean)
1-change[1, -1]/change[2, -1]
round(change[, -1]/npp.hc.kgC_km2_yr * 100)

ggplot(consumption.tot, aes(time, consumption)) +
  #geom_line(data = consumption.tot.sample, aes(time, consumption, group = Q.sample), col = alpha("blue", .1)) +
  geom_point() +
  geom_line() +
  scale_x_reverse("Time (kaBP)") +
  #scale_y_continuous(bquote("Q: Consumption"~(MgCarbon / km^2 / year)), labels = function(x)x/1000) +
  scale_y_log10(bquote("Q: Consumption"~(MgCarbon / km^2 / year)), labels = function(x)x/1000) +
  theme_bw() +
  geom_hline(yintercept = npp.hc.kgC_km2_yr, lty = 2) +
  geom_text(y = log10(npp.hc.kgC_km2_yr), x = 0, label = "Current NPP", hjust = 1, vjust = -1) +
  geom_line(data = ci95, aes(y = median, col = "median"), lwd = 1) +
  geom_line(data = ci95, aes(y = q025, col = "q0.025-0.975"), lty = 2) +
  geom_line(data = ci95, aes(y = q975, col = "q0.025-0.975"), lty = 2) +
  geom_line(data = ci95, aes(y = q25, col = "q25-75"), lty = 3) +
  geom_line(data = ci95, aes(y = q75, col = "q25-75"), lty = 3) +
  geom_line(data = ci95, aes(y = mean, col = "mean"), lwd = 1) +
  geom_line(data = ci95, aes(y = hpd.lwr, col = "HPD95"), lty = 4) +
  geom_line(data = ci95, aes(y = hpd.upr, col = "HPD95"), lty = 4)

ggplot(consumption.tot, aes(time, consumption)) +
  #geom_line(data = consumption.tot.sample, aes(time, consumption, group = Q.sample), col = alpha("blue", .1)) +
  geom_point() +
  geom_line() +
  scale_x_reverse("Time (kaBP)") +
  #scale_y_continuous(bquote("Q: Consumption"~(MgCarbon / km^2 / year)), labels = function(x)x/1000) +
  scale_y_continuous(bquote("Q: Consumption"~(MgCarbon / km^2 / year)), labels = function(x)x/1000) +
  theme_bw() +
  geom_hline(yintercept = npp.hc.kgC_km2_yr, lty = 2) +
  geom_text(y = npp.hc.kgC_km2_yr, x = 0, label = "Current NPP", hjust = 1, vjust = -1) +
  geom_line(data = ci95, aes(y = median, col = "median"), lwd = 1) +
  geom_line(data = ci95, aes(y = q025, col = "q0.025-0.975"), lty = 2) +
  geom_line(data = ci95, aes(y = q975, col = "q0.025-0.975"), lty = 2) +
  geom_line(data = ci95, aes(y = q25, col = "q25-75"), lty = 3) +
  geom_line(data = ci95, aes(y = q75, col = "q25-75"), lty = 3) +
  geom_line(data = ci95, aes(y = mean, col = "mean"), lwd = 1) +
  geom_line(data = ci95, aes(y = hpd.lwr, col = "HPD95"), lty = 4) +
  geom_line(data = ci95, aes(y = hpd.upr, col = "HPD95"), lty = 4)


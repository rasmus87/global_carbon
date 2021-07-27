# Test consumption per species for samples and means
# This script does not need to be run and only tests our sampled distribution and some maths
# 27/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)

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

# Load mean models
consumption.corrected <- read_csv("builds/species.consumption.kgC.yr.km2.csv", col_types = cols()) # [kgC / (km2 * year)]
consumption.naive <- read_csv("builds/species.consumption.kgC.yr.km2.naive.csv", col_types = cols()) # [kgC / (km2 * year)]

# Check consumption models against each other
ggplot(tibble(), aes(x = log10(Q))) +
  geom_density(data = consumption.summary, aes(x = log10(ci.lw), fill = "S.CI"), lty = 2, alpha = .3) +
  geom_density(data = consumption.summary, aes(x = log10(ci.hi), fill = "S.CI"), lty = 2, alpha = .3) +
  geom_density(data = consumption.naive, aes(fill = "Naive"), lty = 3, alpha = .3) +
  geom_density(data = consumption.summary, aes(x = log10(median), fill = "S.median"), lty = 3, alpha = .3) +
  geom_density(data = consumption.corrected, aes(fill = "Corrected"), alpha = .3) +
  geom_density(data = consumption.summary, aes(x = log10(mean), fill = "S.mean"), alpha = .3) +
  theme_bw() +
  scale_fill_discrete("Method")
# Using the naive not log corrected means gives medians while using the corrected version gives means
# We will use medians in the study
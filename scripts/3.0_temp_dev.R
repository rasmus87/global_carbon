library(tidyverse)
library(tictoc)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
load("builds/present.natural.maps.filtered.RData")

# Load Phylacine trait data
df <- read_csv("builds/df.1.1.csv", col_types = cols())

consumption.samples <- read_csv(consumption.samples, "builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols())
consumption <- read_csv(consumption, "builds/species.consumption.kgC.yr.km2.csv", col_types = cols())

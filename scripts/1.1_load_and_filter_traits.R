library(tidyverse)
library(stringr)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.RData")
load("builds/present.natural.maps.RData")

# Load Phylacine trait data
df <- read_csv("../PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())

# Check that maps and traits align
all(df$Binomial.1.2 == rownames(current.maps))
all(df$Binomial.1.2 == rownames(present.natural.maps))

# Load FMR and sample distribution
fmr <- read_csv("../metabolic_rate/builds/imputed.metabolic.rate.csv", col_types = cols())
all(fmr$Binomial.1.2 %in% df$Binomial.1.2)

# Filter df for species which we have FMR for, and check alignment
filter <- which(!df$Binomial.1.2 %in% fmr$Binomial.1.2)
df <- df[-filter, ]
all(df$Binomial.1.2 == fmr$Binomial.1.2)

# Filter maps
current.maps <- current.maps[-filter, ]
present.natural.maps <- present.natural.maps[-filter, ]

# Select important variables from FMR and join to df
fmr <- fmr %>% transmute(Binomial.1.2, log10FMR_est, log10FMR_lwrCI, log10FMR_uprCI)
df <- df %>% left_join(fmr, by = "Binomial.1.2")

# Write builds of the filtered maps
save(current.maps, file = "builds/current.maps.filtered.RData")
save(present.natural.maps, file = "builds/present.natural.maps.filtered.RData")

# Write csv of df
write_csv(df, "builds/df.csv")

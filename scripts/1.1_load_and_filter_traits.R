library(tidyverse)
library(stringr)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.RData")
load("builds/present.natural.maps.RData")

# Load Phylacine trait data and remove all non terrestrial species
df <- read_csv("../PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())
df <- df %>% filter(Marine == 0, Aerial == 0, Terrestrial == 1, Family.1.2 != "Phocidae")
df %>% filter(Freshwater == 1) %>% select(Binomial.1.2, Order.1.2, Family.1.2)

# Load FMR and check that we have FMR for all species
fmr <- read_csv("../metabolic_rate/builds/imputed.metabolic.rate.csv", col_types = cols())
all(df$Binomial.1.2 %in% fmr$Binomial.1.2)

# Select important variables from FMR and join to df
fmr <- fmr %>% transmute(Binomial.1.2, log10FMR.est = log10FMR_est, 
                         se.fmr = (log10FMR_uprCI - log10FMR_lwrCI)/2/qnorm(0.975))
df <- df %>% left_join(fmr, by = "Binomial.1.2")

# Load animal density and check that we have density for all species
dens <- read_csv("../predict_density/output/animal.density.km2.csv", col_types = cols())
all(df$Binomial.1.2 %in% dens$Binomial.1.2)

# Select important variables from dens and join to df
dens <- dens %>% transmute(Binomial.1.2, log10dens.est, 
                         se.dens = se.fit)
df <- df %>% left_join(dens, by = "Binomial.1.2")

# Filter maps
maps <- match(df$Binomial.1.2, rownames(current.maps))
current.maps <- current.maps[maps, ]
present.natural.maps <- present.natural.maps[maps, ]

# Write builds of the filtered maps
save(current.maps, file = "builds/current.maps.filtered.RData")
save(present.natural.maps, file = "builds/present.natural.maps.filtered.RData")

# Write csv of df
write_csv(df, "builds/df.csv")

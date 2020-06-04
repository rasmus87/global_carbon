library(tidyverse)
library(stringr)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.RData")
load("builds/present.natural.maps.RData")

library(tools)
# Data from Phylacine v. 1.2.1
sum(current.maps)
# 1990604
md5sum("builds/current.maps.RData")
# "4669d8c2b5edba3f13ba4bf6d89903b0"
sum(present.natural.maps)
# 2210825
md5sum("builds/present.natural.maps.RData")
# "5bd2cc143ffd61c00aca3f41092c92e"

# Load Phylacine trait data and remove all non terrestrial species
df <- read_csv("../PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())
bat.order <- "Chiroptera"
sea.cow.order <- "Sirenia"
whale.families <- c("Balaenidae", "Balaenopteridae", "Ziphiidae", 
                    "Neobalaenidae", "Delphinidae", "Monodontidae", 
                    "Eschrichtiidae", "Iniidae", "Physeteridae", 
                    "Phocoenidae", "Platanistidae")
seal.families <- c("Otariidae", "Phocidae", "Odobenidae")
marine.carnivores <- c("Enhydra_lutris", "Lontra_felina", "Ursus_maritimus")

terrestrial <- df %>% filter(!Order.1.2 %in% c(bat.order, sea.cow.order),
                             !Family.1.2 %in% c(whale.families, seal.families),
                             !Binomial.1.2 %in% marine.carnivores) %>% pull(Binomial.1.2)

df <- df %>% filter(Binomial.1.2 %in% terrestrial)

# Load FMR and check that we have FMR for all species
fmr <- read_csv("../metabolic_rate/builds/Table S5 Imputed metabolic rate.csv", col_types = cols())
all(df$Binomial.1.2 %in% fmr$Binomial.1.2)

# Select important variables from FMR and join to df
fmr <- fmr %>% transmute(Binomial.1.2, log10fmr = log10.fmr.mean, 
                         se.fmr = sd)
df <- df %>% left_join(fmr, by = "Binomial.1.2")

# Load animal density and check that we have density for all species
dens <- read_csv("../mammal_density/output/Table S4 Imputed density.csv", col_types = cols())
all(df$Binomial.1.2 %in% dens$Binomial.1.2)

# Select important variables from dens and join to df
dens <- dens %>% transmute(Binomial.1.2, log10density = log10.density.mean, 
                         se.dens = sd)
df <- df %>% left_join(dens, by = "Binomial.1.2")

# Select and align maps
maps <- match(df$Binomial.1.2, rownames(current.maps))
current.maps <- current.maps[maps, ]
present.natural.maps <- present.natural.maps[maps, ]
all.equal(df$Binomial.1.2, rownames(current.maps))
all.equal(df$Binomial.1.2, rownames(present.natural.maps))

# Write builds of the filtered maps
save(current.maps, file = "builds/current.maps.filtered.RData")
save(present.natural.maps, file = "builds/present.natural.maps.filtered.RData")

# Write csv of df
write_csv(df, "builds/data.csv")

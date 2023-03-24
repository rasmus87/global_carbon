# Load and filter data to the wanted species list
# 08/03-2023 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)
library(stringr)



# Load PHYLACINE trait data and remove all non terrestrial species ----------

# Load PHYLACINE 1.2.1
df <- read_csv("../PHYLACINE_1.2/Data/Traits/Trait_data.csv", col_types = cols())

# Non terrestrials and humans
bat.order <- "Chiroptera"
sea.cow.order <- "Sirenia"
whale.families <- c("Balaenidae", "Balaenopteridae", "Ziphiidae", 
                    "Neobalaenidae", "Delphinidae", "Monodontidae", 
                    "Eschrichtiidae", "Iniidae", "Physeteridae", 
                    "Phocoenidae", "Platanistidae")
seal.families <- c("Otariidae", "Phocidae", "Odobenidae")
marine.carnivores <- c("Enhydra_lutris", "Lontra_felina", "Ursus_maritimus")
humans <- "Homo"

# Terrestrial species list:
terrestrial <- df %>% 
  filter(!Order.1.2 %in% c(bat.order, sea.cow.order),
         !Family.1.2 %in% c(whale.families, seal.families),
         !Binomial.1.2 %in% marine.carnivores,
         !Genus.1.2 %in% humans) %>% 
  pull(Binomial.1.2)

# Filter traits to the terrestrial list
df <- df %>% filter(Binomial.1.2 %in% terrestrial)
nrow(df)
# [1] 4534


# Load Field Metabolic Rate and Population density -----------------------------

# Load FMR and check that we have FMR for all species
fmr <- read_csv("../metabolic_rate/builds/Imputed metabolic rate.csv", col_types = cols())
all(df$Binomial.1.2 %in% fmr$Binomial.1.2)

# Select important variables from FMR and join to df
fmr <- fmr %>% 
  transmute(Binomial.1.2, 
            log10.fmr.mean,
            se.fmr = sd.fmr,
            fmr.mean,
            fmr.geo.mean)
df <- df %>% left_join(fmr, by = "Binomial.1.2")

# Load animal density and check that we have density for all species
dens <- read_csv("../mammal_density/output/Table S2 Imputed density.csv", col_types = cols())
all(df$Binomial.1.2 %in% dens$Binomial.1.2)

# Select important variables from dens and join to df
dens <- dens %>% 
  transmute(Binomial.1.2, 
            log10.density.mean,
            se.dens = sd,
            density.mean,
            density.geo.mean)
df <- df %>% left_join(dens, by = "Binomial.1.2")

# Write output dataset ----------------------------------------------------
# Write csv of df
write_csv(df, "builds/data.csv")

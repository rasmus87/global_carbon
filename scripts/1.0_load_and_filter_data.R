# Load and filter data to the wanted species list
# 22/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)
library(stringr)

# Load PHYLACINE trait data and remove all non terrestrial speci ----------
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


# Load Field Metabolic Rate and Population density -----------------------------

# Load FMR and check that we have FMR for all species
fmr <- read_csv("../metabolic_rate/builds/Table S5 Imputed metabolic rate.csv", col_types = cols())
all(df$Binomial.1.2 %in% fmr$Binomial.1.2)

# Select important variables from FMR and join to df
fmr <- fmr %>% 
  transmute(Binomial.1.2, 
            log10fmr = log10.fmr.mean, 
            se.fmr = sd)
df <- df %>% left_join(fmr, by = "Binomial.1.2")

# Load animal density and check that we have density for all species
dens <- read_csv("../mammal_density/output/Table S4 Imputed density.csv", col_types = cols())
all(df$Binomial.1.2 %in% dens$Binomial.1.2)

# Select important variables from dens and join to df
dens <- dens %>% 
  transmute(Binomial.1.2, 
            log10density = log10.density.mean, 
            se.dens = sd)
df <- df %>% left_join(dens, by = "Binomial.1.2")

# Load animal density alternative from a PanTHERIA and TetraDENSITY combination
dens.alt <- read_csv("../mammal_density/output/Table S4 Imputed density - PanTetra.csv", col_types = cols())
all(df$Binomial.1.2 %in% dens.alt$Binomial.1.2)

# Select important variables from dens and join to df
dens.alt <- dens.alt %>% 
  transmute(Binomial.1.2, 
            log10density.alt = log10.density.mean, 
            se.dens.alt = sd)
df <- df %>% left_join(dens.alt, by = "Binomial.1.2")

# Write csv of df
write_csv(df, "builds/data.csv")

# Denmark species:
library(tidyverse)
library(raster)

mam <- read_csv("../PHYLACINE_1.2/Data/Traits/Trait_data.csv")

current <- read_rds("../PHYLACINE_1.2_dev/Builds/current.maps.rds")
natural <- read_rds("../PHYLACINE_1.2_dev/Builds/present.natural.maps.rds")

# DK cell = 5231
current.denmark <- current[, 5231]
natural.denmark <- natural[, 5231]

current.species <- names(current.denmark[which(current.denmark != 0)])
natural.species <- names(natural.denmark[which(natural.denmark != 0)])

m.size <- 1.2 # km2
mols <- tibble(Binomial.1.2 = c("Equus_ferus", "Bos_primigenius"),
               Census = c(36, 43),
               Site = "Mols")

df <- left_join(mam, mols)

dens <- read_csv("../predict_density/builds/imputed.density_333.csv")

df <- left_join(df, dens)

df <- df %>% filter(!is.na(Census))

ggplot(df %>% filter(Site == "Mols"), aes(Census, fct_reorder(Binomial.1.2, Mass.g))) +
  #facet_grid(Site ~ ., scales = "free", space = "free") +
  geom_point() +
  scale_x_log10(breaks = c(10^(0:2)), minor_breaks = NULL, labels = c("1", "10", "100")) +
  geom_segment(aes(x = lower.95hpd * m.size, xend = upper.95hpd * m.size, yend = Binomial.1.2)) +
  geom_point(aes(x = density.median), pch = 21) +
  theme_minimal() +
  # theme(strip.background = element_rect(fill = "grey", linetype = 0)) +
  labs(title = "Molslaboratoriet Census",
       subtitle = "Black dots is latest census, lines and circles denote our models predicted density with 95% confidence\n(Predicted density assumes full park coverage)",
       x = "Count",
       y = "Species")
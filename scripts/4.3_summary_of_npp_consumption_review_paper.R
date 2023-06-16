# Summarize Cebrian and  Latigue (2004) review of biomass consumption by animals
# 29/07-2021 Rasmus Ø Pedersen

# Load libraries
library(tidyverse)

# Load data
consumption <- read_tsv("data/consumption_CebrianLatigue2004.txt", na = ".")

# Filter for terrestrial ecosystems
consumption$`ecosystem type` %>% table(useNA = "a")
consumption <- consumption %>% filter(`ecosystem type` == "terrestrial")

# Filter for places with known %NPP consumped
consumption <- consumption %>% filter(!is.na(`%NPP consumed`))

# Table communities
consumption$`community type` %>% table

# Look at the numbers
consumption$`%NPP consumed` %>% summary

# Sort for above, below or both
any(str_detect(consumption$reference, "\\*\\*"))
consumption$compartment <- ifelse(str_detect(consumption$reference, "\\*"), "above.and.below", "above.only")

# Summarise consumption by strata
consumption %>% 
  group_by(compartment) %>% 
  summarise(med = median(`%NPP consumed`),
            q75 = quantile(`%NPP consumed`, 0.75),
            min = min(`%NPP consumed`),
            max = max(`%NPP consumed`),
            n = n())

# Make a histogram of the data
ggplot(consumption, aes(`%NPP consumed`, fill = compartment)) +
  geom_histogram(breaks = seq(0,80,5)) +
  theme_bw() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

# Make a density plot of the dat
ggplot(consumption, aes(`%NPP consumed`, color = compartment)) +
  geom_density() +
  theme_bw() +
  geom_rug() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

# Above only density plot
ggplot(consumption %>% filter(compartment == "above.and.below"), aes(`%NPP consumed`, color = compartment)) +
  geom_density() +
  theme_bw() +
  geom_rug() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

# Summarise above only data
consumption %>% 
  filter(compartment == "above.and.below") %>%
  summarise(min = min(`%NPP consumed`),
            q025 = quantile(`%NPP consumed`, 0.025),
            q25 = quantile(`%NPP consumed`, 0.25),
            mean = mean(`%NPP consumed`),
            med = median(`%NPP consumed`),
            q75 = quantile(`%NPP consumed`, 0.75),
            q975 = quantile(`%NPP consumed`, 0.975),
            max = max(`%NPP consumed`),
            n = n()) %>% 
  signif(2)

# Make a histogram of the data
consumption %>% 
  filter(compartment == "above.and.below") %>%
  ggplot(aes(`%NPP consumed`, fill = `community type`)) +
    geom_histogram(aes(y = ..density../17), breaks = seq(0,30,1)) +
    theme_bw() +
    scale_fill_discrete(name = NULL) +
    scale_y_continuous(breaks = seq(0, 10, 3)) +
    geom_density(data = ltw %>% filter(period == "Current"), aes(x = value, fill = NULL, col = "Current estimate"), fill = NA) +
  scale_color_manual(name = NULL, values = "black") +
  theme(legend.position = c(.75, .75))

ltw %>% 
  filter(period == "Current") %>%
  summarise(min = min(value),
            q025 = quantile(value, 0.025),
            q25 = quantile(value, 0.25),
            mean = mean(value),
            med = median(value),
            q75 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            max = max(value),
            n = n()) %>% 
  signif(2)

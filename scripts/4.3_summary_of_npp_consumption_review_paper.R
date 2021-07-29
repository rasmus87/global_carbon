library(tidyverse)

consumption <- read_tsv("data/consumption_CebrianLatigue2004.txt", na = ".")

consumption$`ecosystem type` %>% table(useNA = "a")

consumption <- consumption %>% filter(`ecosystem type` == "terrestrial")

consumption <- consumption %>% filter(!is.na(`%NPP consumed`))

consumption$`community type` %>% table

consumption$`%NPP consumed`

any(str_detect(consumption$reference, "\\*\\*"))

consumption$compartment <- ifelse(str_detect(consumption$reference, "\\*"), "above.below", "above.only")

consumption %>% 
  group_by(compartment) %>% 
  summarise(med = median(`%NPP consumed`),
            q75 = quantile(`%NPP consumed`, 0.75),
            min = min(`%NPP consumed`),
            max = max(`%NPP consumed`),
            n = n())


ggplot(consumption, aes(`%NPP consumed`, fill = compartment)) +
  geom_histogram(breaks = seq(0,80,5)) +
  theme_bw() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

ggplot(consumption, aes(`%NPP consumed`, color = compartment)) +
  geom_density() +
  theme_bw() +
  geom_rug() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

ggplot(consumption %>% filter(compartment == "above.below"), aes(`%NPP consumed`, color = compartment)) +
  geom_density() +
  theme_bw() +
  geom_rug() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))


consumption %>% 
  filter(compartment == "above.below") %>%
  summarise(min = min(`%NPP consumed`),
            q025 = quantile(`%NPP consumed`, 0.025),
            q25 = quantile(`%NPP consumed`, 0.25),
            mean = mean(`%NPP consumed`),
            med = median(`%NPP consumed`),
            q75 = quantile(`%NPP consumed`, 0.75),
            q975 = quantile(`%NPP consumed`, 0.975),
            max = max(`%NPP consumed`),
            n = n())


library(tidyverse)

consumption <- read_tsv("data/consumption_CebrianLatigue2004.txt", na = ".")

consumption$`ecosystem type` %>% table(useNA = "a")

consumption <- consumption %>% filter(`ecosystem type` == "terrestrial")

consumption <- consumption %>% filter(!is.na(`%NPP consumed`))

consumption$`community type` %>% table

consumption$`%NPP consumed`

any(str_detect(consumption$reference, "\\*\\*"))

consumption$compartment <- ifelse(str_detect(consumption$reference, "\\*"), "above.below", "above.only")

tab <- consumption %>% group_by(compartment) %>% summarise(med = median(`%NPP consumed`),
                                                           q75 = quantile(`%NPP consumed`, 0.75),
                                                           min = min(`%NPP consumed`),
                                                           max = max(`%NPP consumed`),
                                                           n = n())
tab

ggplot(consumption, aes(`%NPP consumed`, fill = compartment)) +
  geom_histogram(breaks = seq(0,80,5)) +
  theme_bw() +
  scale_fill_discrete(name = NULL, labels = c("Total NPP", "Aboveground only NPP")) +
  theme(legend.position = c(.75, .75))

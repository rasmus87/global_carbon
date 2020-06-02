library(tidyverse)
library(tictoc)
library(data.table)
library(raster)

# Load map data (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
load("builds/present.natural.maps.filtered.RData")

# Load Phylacine trait data
df <- read_csv("builds/df.1.1.csv", col_types = cols())

# Load consumption sampled dataset
consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols())

# Check that we are aligned
all(consumption.samples$Binomial.1.2 == df$Binomial.1.2)


consumption <- read_csv("builds/species.consumption.kgC.yr.km2.csv", col_types = cols())
consumption.naive <- read_csv("builds/species.consumption.kgC.yr.km2.naive.csv", col_types = cols())

all(consumption$Binomial.1.2 == df$Binomial.1.2)
consumption0 <- consumption %>% mutate(Q.plant = Q * df$Diet.Plant/100)

consumption.summary <- consumption.samples %>% 
  transmute(Binomial.1.2, 
            mean = rowMeans(.[, -1]),
            median = apply(.[, -1], 1, median),
            Q05 = apply(.[, -1], 1, quantile, probs = .05),
            Q95 = apply(.[, -1], 1, quantile, probs = .95))
#
consumption <- consumption.summary %>% mutate(Q.plant = median * df$Diet.Plant/100,
                                              Q05.plant = Q05 * df$Diet.Plant/100,
                                              Q95.plant = Q95 * df$Diet.Plant/100)

current.consumption <- current.maps * consumption$Q.plant
present.natural.consumption <- present.natural.maps * consumption$Q.plant

current.consumption.05 <- current.maps * consumption$Q05.plant
present.natural.consumption.05 <- present.natural.maps * consumption$Q05.plant
current.consumption.95 <- current.maps * consumption$Q95.plant
present.natural.consumption.95 <- present.natural.maps * consumption$Q95.plant

base.map <- raster("builds/base_map.tif")

current.consumption.map <- base.map
present.natural.consumption.map <- base.map
current.consumption.map.05 <- base.map
present.natural.consumption.map.05 <- base.map
current.consumption.map.95 <- base.map
present.natural.consumption.map.95 <- base.map

tic()
current.consumption.map[] <- colSums(current.consumption)
current.consumption.map[current.consumption.map == 0] <- NA

current.consumption.map.05[] <- colSums(current.consumption.05)
current.consumption.map.05[current.consumption.map.05 == 0] <- NA
current.consumption.map.95[] <- colSums(current.consumption.95)
current.consumption.map.95[current.consumption.map.95 == 0] <- NA
toc()
tic()
present.natural.consumption.map[] <- colSums(present.natural.consumption)
present.natural.consumption.map[present.natural.consumption.map == 0] <- NA

present.natural.consumption.map.05[] <- colSums(present.natural.consumption.05)
present.natural.consumption.map.05[present.natural.consumption.map.05 == 0] <- NA
present.natural.consumption.map.95[] <- colSums(present.natural.consumption.95)
present.natural.consumption.map.95[present.natural.consumption.map.95 == 0] <- NA
toc()

#### TEST TOTALT CONSUMPTION
tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T)*10^3/10^15
tot.current
tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat
tot.pres.nat - tot.current

tot.current.05 <- sum(current.consumption.map.05[] * prod(res(current.consumption.map.05))/10^6, na.rm = T)*10^3/10^15
tot.current.05
tot.pres.nat.05 <- sum(present.natural.consumption.map.05[] * prod(res(current.consumption.map.05))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat.05
tot.pres.nat.05 - tot.current.05

tot.current.95 <- sum(current.consumption.map.95[] * prod(res(current.consumption.map.95))/10^6, na.rm = T)*10^3/10^15
tot.current.95
tot.pres.nat.95 <- sum(present.natural.consumption.map.95[] * prod(res(current.consumption.map.95))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat.95
tot.pres.nat.95 - tot.current.95
#### END TEST

# Carbon consumption maps:
current.consumption.map.spdf <- as(current.consumption.map, "SpatialPixelsDataFrame")
current.consumption.map.df <- as_data_frame(current.consumption.map.spdf)
colnames(current.consumption.map.df) <- c("value", "x", "y")
current.consumption.map.df <- current.consumption.map.df %>% mutate(group = "Current")
present.natural.consumption.map.spdf <- as(present.natural.consumption.map, "SpatialPixelsDataFrame")
present.natural.consumption.map.df <- as_data_frame(present.natural.consumption.map.spdf)
colnames(present.natural.consumption.map.df) <- c("value", "x", "y")
present.natural.consumption.map.df <- present.natural.consumption.map.df %>% mutate(group = "Present natural")
consumption.map.df <- bind_rows(current.consumption.map.df, present.natural.consumption.map.df)
consumption.map.df$group <- fct_relevel(consumption.map.df$group, "Present natural")
consumption.plot <- ggplot(consumption.map.df, aes(x = x, y = y, fill = value/10^3)) +
  facet_grid(group ~ .) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white") +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold"))

# Change in consumption
change <- (current.consumption.map/present.natural.consumption.map - 1)*100
change[change > 0] <- 0
change.spdf <- as(change, "SpatialPixelsDataFrame")
change.df <- as_data_frame(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$group <- "Difference"
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(group ~ .) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(name = Difference~('%'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold"))
change.plot

g1 <- ggplotGrob(consumption.plot)
g2 <- ggplotGrob(change.plot)
p1 <- gtable_rbind(g1, g2)
arrangeGrob(p1) %>% plot
ggsave("./output/fig1.png", p1, width = 20, height = 23, units = "cm")


npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif")
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA
npp[] <- npp[] * 1000^2 / 1000 # kg Carbon / km2 / yr
npp[] <- npp[] / 1000 # Mg Carbon / km2 / yr

#### TEST TOTALT NPP
tot.npp <- sum(npp[] * prod(res(npp))/10^6, na.rm = T)*10^6/10^15
tot.npp
# Percentage released (Doughty says 2.2-5.3)
(tot.pres.nat - tot.current)/tot.npp * 100

(tot.pres.nat.05 - tot.current.05)/tot.npp * 100
(tot.pres.nat.95 - tot.current.95)/tot.npp * 100


#### END TEST


current.consumption.map[cell]/1000
present.natural.consumption.map[cell]/1000
npp[cell]
current.npp.use[cell]
present.natural.npp.use[cell]

npp[which(npp[] == 0)] <- NA
current.npp.use <- (current.consumption.map/1000)/npp * 100
present.natural.npp.use <- (present.natural.consumption.map/1000)/npp * 100
current.npp.use.org <- current.npp.use
present.natural.npp.use.org <- present.natural.npp.use

current.npp.use[current.npp.use >= 100.5] <- 101
present.natural.npp.use[present.natural.npp.use >= 100.5] <- 101

current.npp.use.spdf <- as(current.npp.use, "SpatialPixelsDataFrame")
current.npp.use.df <- as_data_frame(current.npp.use.spdf)
colnames(current.npp.use.df) <- c("value", "x", "y")
present.natural.npp.use.spdf <- as(present.natural.npp.use, "SpatialPixelsDataFrame")
present.natural.npp.use.df <- as_data_frame(present.natural.npp.use.spdf)
colnames(present.natural.npp.use.df) <- c("value", "x", "y")

pn.npp.use <- present.natural.npp.use.df %>% mutate(group = "Present natural")
cu.npp.use <- current.npp.use.df %>% mutate(group = "Current")
npp.use <- bind_rows(pn.npp.use, cu.npp.use)
npp.use$group <- as.factor(npp.use$group)
npp.use$group <- fct_relevel(npp.use$group, "Present natural")
frac.npp.consumption.plot <- ggplot(npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(group ~ .) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis(name = Consumption~of~current~NPP~("%"),
                     na.value = "white") +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold"))

# Difference in consumption
change2 <- current.npp.use - present.natural.npp.use
change2[change2 > 0] <- 0
change.spdf <- as(change2, "SpatialPixelsDataFrame")
change.df <- as_data_frame(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$group <- "Percentage point difference"
pct.pt.diffrence.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(group ~ .) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold"))

g3 <- ggplotGrob(frac.npp.consumption.plot)
g4 <- ggplotGrob(pct.pt.diffrence.plot)
p2 <- gtable_rbind(g3, g4)
arrangeGrob(p2) %>% plot
ggsave("./output/fig2.png", p2, width = 20, height = 23, units = "cm")

npp.use$value[npp.use$value == 101] <- NA
ggplot(npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(group ~ .) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis(name = Consumption~of~current~NPP~("%"),
                     na.value = "hotpink") +
  theme_map()
ggsave("./output/fig.S1.png", width = 20, height = 15, units = "cm")

# Load continents
continents <- raster("data/Continents for terrestrial mammals.tif")
plot(continents, col = rainbow(n = 6))
continents <- as(continents, "SpatialPixelsDataFrame")
continents <- as_data_frame(continents)
colnames(continents) <- c("value", "x", "y")
continents <- continents %>% 
  transmute(x,y, continent = fct_recode(value %>% as.factor, 
                                        "North~America" = "2", "South~America" = "5", 
                                        "Europe" = "3", "Africa" = "4", "Asia" = "1",
                                        "Oceania" = "6"))

summary <- continents %>% 
  left_join(current.consumption.map.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")

ggplot(summary, aes(continent, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "")
summary %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))


summary <- continents %>% 
  left_join(npp.use, by = c("x", "y")) %>% 
  na.omit()
cont.npp.use.plot <- ggplot(summary, aes(continent, value, fill = group)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "")
m <- glm(value ~ continent, data = summary)
summary(m)
glob.npp.use.plot <- ggplot(summary, aes(group, value, fill = group)) +
  geom_boxplot() +
  theme_bw() +
  ylab() +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.y = element_blank())   
m <- glm(value ~ group, data = summary)
summary(m)
summary %>% group_by(group) %>% summarise(median(value, na.rm= T))
summary %>% group_by(group) %>% summarise(quantile(value, .025, na.rm= T))
summary %>% group_by(group) %>% summarise(quantile(value, .975, na.rm= T))
summary %>% group_by(group) %>% summarise(mean(value, na.rm= T))
summary %>% group_by(group) %>% summarise(sd(value, na.rm= T))

summary2 <- summary
summary2$overgroup <- "Continent"
summary3 <- summary2
summary3$overgroup <- "Global"
summary3$continent <- "Global"
summary4 <- rbind(summary2, summary3)
ggplot(summary4, aes(continent, value, fill = group)) +
  facet_grid(. ~ overgroup, scale = "free", space = "free", labeller = label_parsed) +
  geom_boxplot() +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab(NULL) +
  scale_fill_brewer(type = "qual", name = "") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggplot(summary4, aes(continent, value, fill = group)) +
  facet_grid(. ~ overgroup, scale = "free", space = "free", labeller = label_parsed) +
  geom_violin(draw_quantiles = c(.5), width = .65, scale = "width") +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab(NULL) +
  scale_fill_brewer(type = "qual", name = "") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave("./output/fig3.png", width = 20, height = 10, units = "cm")

######### Consumption by realm and biome >>>
base.map <- raster("builds/base_map.tif")
ltw.biome <- base.map
ltw.biome[] <- raster("data/ltw_biome.tif")[]
ltw.biome[ltw.biome[] == 128] <- NA
plot(ltw.biome, col = rainbow(n = 13))
ltw.biome <- as(ltw.biome, "SpatialPixelsDataFrame")
ltw.biome <- as_data_frame(ltw.biome)
colnames(ltw.biome) <- c("value", "x", "y")
names <- read_csv("data/ltw_biome_names.csv")
ltw.biome <- left_join(ltw.biome, names, by = c("value" = "ltw_biome"))
summary <- ltw.biome %>% 
  transmute(x,y, ltw.biome = as.factor(name))
summary <- summary %>% 
  left_join(current.consumption.map.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")

ggplot(summary, aes(ltw.biome, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))

ltw.realm <- base.map
ltw.realm[] <- raster("data/ltw_realm.tif")[]
plot(ltw.realm, col = rainbow(n = 8))
ltw.realm <- as(ltw.realm, "SpatialPixelsDataFrame")
ltw.realm <- as_data_frame(ltw.realm)
colnames(ltw.realm) <- c("value", "x", "y")
names <- read_csv("data/ltw_realm_names.csv")
ltw.realm <- left_join(ltw.realm, names, by = c("value" = "ltw_realm"))
summary <- ltw.realm %>% 
  transmute(x,y, ltw.realm = as.factor(name))
summary <- summary %>% 
  left_join(current.consumption.map.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")

ggplot(summary, aes(ltw.realm, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))



summary <- current.consumption.map.df %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")

ggplot(summary, aes(period, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))
m <- glm(NPP.consumption ~ period, data = summary)
summary(m)
summary %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))
######### Consumption by realm and biome |||


######### NPP use by realm and biome >>>
current.npp.use.org.spdf <- as(current.npp.use.org, "SpatialPixelsDataFrame")
current.npp.use.org.df <- as_data_frame(current.npp.use.org.spdf)
colnames(current.npp.use.org.df) <- c("value", "x", "y")
present.natural.npp.use.org.spdf <- as(present.natural.npp.use.org, "SpatialPixelsDataFrame")
present.natural.npp.use.org.df <- as_data_frame(present.natural.npp.use.org.spdf)
colnames(present.natural.npp.use.org.df) <- c("value", "x", "y")

ltw.realm <- base.map
ltw.realm[] <- raster("data/ltw_realm.tif")[]
ltw.realm <- as(ltw.realm, "SpatialPixelsDataFrame")
ltw.realm <- as_data_frame(ltw.realm)
colnames(ltw.realm) <- c("value", "x", "y")
names <- read_csv("data/ltw_realm_names.csv")
ltw.realm <- left_join(ltw.realm, names, by = c("value" = "ltw_realm"))
summary <- ltw.realm %>% 
  transmute(x,y, ltw = as.factor(name))
summary <- summary %>% 
  left_join(current.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")
summary.realm <- summary

# ggplot(summary, aes(ltw, NPP.consumption, fill = period)) +
#   geom_boxplot(notch = TRUE) +
#   theme_bw() +
#   ylab("NPP use (%)") +
#   xlab(NULL) +
#   scale_fill_brewer(type = "qual", name = "") +
#   coord_cartesian(ylim = c(0, 100)) +
#   labs(subtitle = "a) Realm") +
#   theme(plot.subtitle = element_text(face = "bold"))
glm(NPP.consumption ~ period, data = summary)
summary %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))
summary %>% group_by(period) %>% summarise(quantile(NPP.consumption, .025, na.rm= T))
summary %>% group_by(period) %>% summarise(quantile(NPP.consumption, .975, na.rm= T))
summary %>% group_by(period) %>% summarise(mean(NPP.consumption, na.rm= T))
summary %>% group_by(period) %>% summarise(sd(NPP.consumption, na.rm= T))

ltw.biome <- base.map
ltw.biome[] <- raster("data/ltw_biome.tif")[]
ltw.biome[ltw.biome[] == 128] <- NA
ltw.biome <- as(ltw.biome, "SpatialPixelsDataFrame")
ltw.biome <- as_data_frame(ltw.biome)
colnames(ltw.biome) <- c("value", "x", "y")
names <- read_csv("data/ltw_biome_names.csv")
ltw.biome <- left_join(ltw.biome, names, by = c("value" = "ltw_biome"))
summary <- ltw.biome %>% 
  transmute(x,y, ltw = as.factor(name))
summary <- summary %>% 
  left_join(current.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")
summary.biome <- summary

# ggplot(summary, aes(ltw, NPP.consumption, fill = period)) +
#   geom_boxplot(notch = FALSE) +
#   theme_bw() +
#   ylab("NPP use (%)") +
#   xlab(NULL) +
#   scale_fill_brewer(type = "qual", name = "") +
#   coord_cartesian(ylim = c(0, 100)) +
#   theme(axis.text.x=element_text(angle=30, hjust=1)) +
#   labs(subtitle = "b) Biome") +
#   theme(plot.subtitle = element_text(face = "bold"))



summary.realm$group <- "Realm"
summary.biome$group <- "Biome"
summary.ltw <- bind_rows(summary.realm, summary.biome)
summary.ltw$group <- as_factor(summary.ltw$group)
summary.ltw <- na.omit(summary.ltw)
ggplot(summary.ltw, aes(ltw, NPP.consumption, fill = period)) +
  facet_grid(. ~ group, scale = "free", space = "free", labeller = label_parsed) +
  geom_boxplot() +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab(NULL) +
  scale_fill_brewer(type = "qual", name = NULL) +
  theme(legend.position="bottom") +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text.x=element_text(angle=30, hjust=1),
        plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"),
        legend.box.spacing = unit(c(0,0,0,0), "pt"))
ggsave("./output/fig4.png", width = 25, height = 18, units = "cm")


ggplot(summary.biome, aes(ltw, NPP.consumption, fill = period)) +
  geom_boxplot(notch = FALSE) +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  scale_y_log10() +
  theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=0.8))















# IBS TEST
######### NPP use by realm and biome >>>
current.npp.use.org.spdf <- as(current.npp.use.org, "SpatialPixelsDataFrame")
current.npp.use.org.df <- as_data_frame(current.npp.use.org.spdf)
colnames(current.npp.use.org.df) <- c("value", "x", "y")
present.natural.npp.use.org.spdf <- as(present.natural.npp.use.org, "SpatialPixelsDataFrame")
present.natural.npp.use.org.df <- as_data_frame(present.natural.npp.use.org.spdf)
colnames(present.natural.npp.use.org.df) <- c("value", "x", "y")

ltw.realm <- base.map
ltw.realm[] <- raster("data/ltw_realm.tif")[]
ltw.realm <- as(ltw.realm, "SpatialPixelsDataFrame")
ltw.realm <- as_data_frame(ltw.realm)
colnames(ltw.realm) <- c("value", "x", "y")
names <- read_csv("data/ltw_realm_names.csv")
ltw.realm <- left_join(ltw.realm, names, by = c("value" = "ltw_realm"))
summary <- ltw.realm %>% 
  transmute(x,y, ltw = as.factor(name))
summary <- summary %>% left_join(continents)
summary <- summary %>% 
  left_join(current.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y"))
summary <- summary %>% gather(key = "period", value = "NPP.consumption", value.x, value.y)
summary <- summary %>% mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x"))
summary$period <- fct_relevel(summary$period, "Present Natural")
summary.realm <- summary

glm(NPP.consumption ~ period, data = summary)
summary %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))
summary %>% group_by(period) %>% summarise(quantile(NPP.consumption, .025, na.rm= T))
summary %>% group_by(period) %>% summarise(quantile(NPP.consumption, .975, na.rm= T))
summary %>% group_by(period) %>% summarise(mean(NPP.consumption, na.rm= T))
summary %>% group_by(period) %>% summarise(sd(NPP.consumption, na.rm= T))

summary2 <- summary
summary2$overgroup <- "Continent"
summary3 <- summary2
summary3$overgroup <- "Global"
summary3$continent <- "Global"
summary4 <- rbind(summary2, summary3)

ggplot(summary4 %>% filter(NPP.consumption < 100), aes(continent, NPP.consumption, fill = period)) +
  facet_grid(. ~ overgroup, scale = "free", space = "free", labeller = label_parsed) +
  geom_violin(draw_quantiles = c(.5), width = .65, scale = "width") +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab(NULL) +
  scale_fill_brewer(type = "qual", name = "") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave("./output/fig_ltw.png", width = 20, height = 10, units = "cm")



ggplot(continents, aes(x, y, fill = continent)) +
  geom_tile(show.legend = FALSE) + 
  coord_equal() +
  theme_map()


ggplot(summary, aes(x, y, fill = continent)) +
  geom_tile(show.legend = FALSE) + 
  coord_equal() +
  theme_map()

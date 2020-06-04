library(tidyverse)
library(tictoc)
library(data.table)
library(raster)
library(viridis)  # better colors for everyone
library(ggthemes)
library(gridExtra)


# Load Phylacine trait data
df <- read_csv("builds/data.csv", col_types = cols())

consumption.samples <- read_csv("builds/sampled.consumption.distribution.kgC.yr.km2.csv", col_types = cols())
consumption.corrected <- read_csv("builds/species.consumption.kgC.yr.km2.csv", col_types = cols())
consumption.naive <- read_csv("builds/species.consumption.kgC.yr.km2.naive.csv", col_types = cols())

all(consumption$Binomial.1.2 == df$Binomial.1.2)
consumption.corrected <- consumption.corrected %>% mutate(Q.plant = Q * df$Diet.Plant/100)

consumption.summary <- consumption.samples %>% 
  transmute(Binomial.1.2, 
            mean = rowMeans(.[, -1]),
            median = apply(.[, -1], 1, median),
            ci.lw = apply(.[, -1], 1, quantile, probs = .025),
            ci.hi = apply(.[, -1], 1, quantile, probs = .975))

ggplot(tibble(), aes(x = log10(Q))) +
  geom_density(data = consumption.naive, col = "yellow") +
  geom_density(data = consumption.corrected, col = "magenta") +
  geom_density(data = consumption.summary, aes(x = log10(mean)), col = "red") +
  geom_density(data = consumption.summary, aes(x = log10(median)), col = "darkgreen") +
  geom_density(data = consumption.summary, aes(x = log10(ci.lw)), col = "darkgreen", lty = 2) +
  geom_density(data = consumption.summary, aes(x = log10(ci.hi)), col = "darkgreen", lty = 2) +
  theme_bw()

all(consumption.summary$Binomial.1.2 == df$Binomial.1.2)
consumption <- consumption.summary %>% mutate(Q.plant = median * df$Diet.Plant/100,
                                              ci.lw.plant = ci.lw * df$Diet.Plant/100,
                                              ci.hi.plant = ci.hi * df$Diet.Plant/100)


# Load map data and builds consumption maps (this has to ben run after the load.maps.R script)
load("builds/current.maps.filtered.RData")
current.consumption <- current.maps * consumption$Q.plant
current.consumption.lw <- current.maps * consumption$ci.lw.plant
current.consumption.hi <- current.maps * consumption$ci.hi.plant
rm(current.maps) # Each loaded matrix is 1.7 GB ram
gc() # R forgets to clean up

load("builds/present.natural.maps.filtered.RData")
present.natural.consumption <- present.natural.maps * consumption$Q.plant
present.natural.consumption.lw <- present.natural.maps * consumption$ci.lw.plant
present.natural.consumption.hi <- present.natural.maps * consumption$ci.hi.plant
rm(present.natural.maps) # Each loaded matrix is 1.7 GB ram
gc() # R forgets to clean up

base.map <- raster("builds/base_map.tif")

current.consumption.map <- base.map
present.natural.consumption.map <- base.map
current.consumption.map.lw <- base.map
present.natural.consumption.map.lw <- base.map
current.consumption.map.hi <- base.map
present.natural.consumption.map.hi <- base.map

tic()
current.consumption.map[] <- colSums(current.consumption)
current.consumption.map[current.consumption.map == 0] <- NA

current.consumption.map.lw[] <- colSums(current.consumption.lw)
current.consumption.map.lw[current.consumption.map.lw == 0] <- NA
current.consumption.map.hi[] <- colSums(current.consumption.hi)
current.consumption.map.hi[current.consumption.map.hi == 0] <- NA
toc()
tic()
present.natural.consumption.map[] <- colSums(present.natural.consumption)
present.natural.consumption.map[present.natural.consumption.map == 0] <- NA

present.natural.consumption.map.lw[] <- colSums(present.natural.consumption.lw)
present.natural.consumption.map.lw[present.natural.consumption.map.lw == 0] <- NA
present.natural.consumption.map.hi[] <- colSums(present.natural.consumption.hi)
present.natural.consumption.map.hi[present.natural.consumption.map.hi == 0] <- NA
toc()

#### TEST TOTALT CONSUMPTION
tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T)*10^3/10^15
tot.current
tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat
tot.pres.nat - tot.current

tot.current.lw <- sum(current.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T)*10^3/10^15
tot.current.lw
tot.pres.nat.lw <- sum(present.natural.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat.lw
tot.pres.nat.lw - tot.current.lw

tot.current.hi <- sum(current.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T)*10^3/10^15
tot.current.hi
tot.pres.nat.hi <- sum(present.natural.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T)*10^3/10^15
tot.pres.nat.hi
tot.pres.nat.hi - tot.current.hi
#### END TEST


# Carbon consumption maps:
library(rworldmap)
library(maptools)
newmap <- getMap(resolution = "low")
newmap <- unionSpatialPolygons(newmap, rep(1, nrow(newmap)))
newmap <- spTransform(newmap, crs(current.consumption.map))
newmap <- fortify(newmap)
ggplot() +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = NA)

current.consumption.map.spdf <- as(current.consumption.map, "SpatialPixelsDataFrame")
current.consumption.map.df <- as_data_frame(current.consumption.map.spdf)
colnames(current.consumption.map.df) <- c("value", "x", "y")
current.consumption.map.df <- current.consumption.map.df %>% mutate(time = "Current")
present.natural.consumption.map.spdf <- as(present.natural.consumption.map, "SpatialPixelsDataFrame")
present.natural.consumption.map.df <- as_data_frame(present.natural.consumption.map.spdf)
colnames(present.natural.consumption.map.df) <- c("value", "x", "y")
present.natural.consumption.map.df <- present.natural.consumption.map.df %>% mutate(time = "Present natural")
consumption.map.df <- bind_rows(current.consumption.map.df, present.natural.consumption.map.df)

current.consumption.plot <- ggplot(current.consumption.map.df, aes(x = x, y = y, fill = value/10^3)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)/10^3) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

present.natural.consumption.plot <- ggplot(present.natural.consumption.map.df, aes(x = x, y = y, fill = value/10^3)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_viridis(name = Consumption~(MgC/yr/km^2),
                     na.value = "white",
                     limits = range(consumption.map.df$value)/10^3) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) + 
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Change in consumption
change <- (current.consumption.map/present.natural.consumption.map - 1)*100
change[change > 0] <- 0
change.spdf <- as(change, "SpatialPixelsDataFrame")
change.df <- as_data_frame(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$time <- "Difference"
change.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(current.consumption.map.df$y)) +
  scale_fill_gradientn(name = Difference~('%'),
                    na.value = "white",
                    colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

g1 <- ggplotGrob(current.consumption.plot)
g2 <- ggplotGrob(present.natural.consumption.plot)
g3 <- ggplotGrob(change.plot)
p1 <- gtable_rbind(g1, g2, g3)
arrangeGrob(p1) %>% plot
ggsave("./output/fig1_carbon_consumption.png", p1, width = 20, height = 23, units = "cm")


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

(tot.pres.nat.lw - tot.current.lw)/tot.npp * 100
(tot.pres.nat.hi - tot.current.hi)/tot.npp * 100
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

pn.npp.use <- present.natural.npp.use.df %>% mutate(time = "Present natural")
cu.npp.use <- current.npp.use.df %>% mutate(time = "Current")
npp.use <- bind_rows(pn.npp.use, cu.npp.use)
npp.use$time <- as.factor(npp.use$time)
npp.use$time <- fct_relevel(npp.use$time, "Present natural")

frac.npp.cu.consumption.plot <- ggplot(cu.npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "grey",
                     limits = range(npp.use$value)) +
  theme_map() +
  labs(subtitle = "a") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

cells <- change.df
cells$value <- NA
cells$time <- "Present natural"
pn.npp.use2 <- bind_rows(pn.npp.use, cells)
pn.npp.use2 <- pn.npp.use2 %>% distinct(x, y, .keep_all = TRUE)

frac.npp.pn.consumption.plot <- ggplot(pn.npp.use2, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "Consumption of\ncurrent NPP (%)",
                     na.value = "white",
                     limits = range(npp.use$value)) +
  theme_map() +
  labs(subtitle = "b") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Difference in consumption
change2 <- current.npp.use - present.natural.npp.use
change2[change2 > 0] <- 0
change.spdf <- as(change2, "SpatialPixelsDataFrame")
change.df <- as_data_frame(change.spdf)
colnames(change.df) <- c("value", "x", "y")
change.df$time <- "Percentage point difference"
pct.pt.diffrence.plot <- ggplot(change.df, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_gradientn(name = Difference~('%-point'),
                       na.value = "white",
                       colours = plasma(10)) +
  theme_map() +
  labs(subtitle = "c") +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

g4 <- ggplotGrob(frac.npp.cu.consumption.plot)
g5 <- ggplotGrob(frac.npp.pn.consumption.plot)
g6 <- ggplotGrob(pct.pt.diffrence.plot)
p2 <- gtable_rbind(g4, g5, g6)
arrangeGrob(p2) %>% plot
ggsave("./output/fig2_fraction_npp_consumed.png", p2, width = 20, height = 23, units = "cm")

npp.use$value[npp.use$value == 101] <- NA
ggplot(npp.use, aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = Consumption~of~current~NPP~("%"),
                     na.value = "hotpink") +
  theme_map() +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)
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
glob.npp.use.plot <- ggplot(summary, aes(time, value, fill = time)) +
  geom_boxplot() +
  theme_bw() +
  ylab("") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.y = element_blank())   
m <- glm(value ~ time, data = summary)
summary(m)
summary %>% group_by(time) %>% summarise(median(value, na.rm= T))
summary %>% group_by(time) %>% summarise(quantile(value, .025, na.rm= T))
summary %>% group_by(time) %>% summarise(quantile(value, .975, na.rm= T))
summary %>% group_by(time) %>% summarise(mean(value, na.rm= T))
summary %>% group_by(time) %>% summarise(sd(value, na.rm= T))

summary2 <- summary
summary2$overgroup <- "Continent"
summary3 <- summary2
summary3$overgroup <- "Global"
summary3$continent <- "Global"
summary4 <- rbind(summary2, summary3)
ggplot(summary4, aes(continent, value, fill = time)) +
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
ggplot(summary4, aes(continent, value, fill = time)) +
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

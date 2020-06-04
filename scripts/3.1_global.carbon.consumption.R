# Run after 3.0_load.data.R

### CACULATE TOTALT GLOBAL CONSUMPTION >>>
# Units:  
# [kgC / (km2 * year) * m2 * km2 / 10^6 m2 * 10^3 g / kg * Pg / 10^15 g] = 
# [PgC / year]
tot.current <- sum(current.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^3 / 10^15
tot.pres.nat <- sum(present.natural.consumption.map[] * prod(res(current.consumption.map))/10^6, na.rm = T) * 10^3 / 10^15
diff <- tot.pres.nat - tot.current

tot.current.lw <- sum(current.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^3 / 10^15
tot.pres.nat.lw <- sum(present.natural.consumption.map.lw[] * prod(res(current.consumption.map.lw))/10^6, na.rm = T) * 10^3 / 10^15
diff.lw <- tot.pres.nat.lw - tot.current.lw

tot.current.hi <- sum(current.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^3 / 10^15
tot.pres.nat.hi <- sum(present.natural.consumption.map.hi[] * prod(res(current.consumption.map.hi))/10^6, na.rm = T) * 10^3 / 10^15
diff.hi <- tot.pres.nat.hi - tot.current.hi

paste0("Current consumption: ", round(tot.current), " Pg Carbon / year (95%-CI: ", round(tot.current.lw), "-", round(tot.current.hi) ,")")
paste0("Present natural consumption: ", round(tot.pres.nat), " Pg Carbon / year (95%-CI: ", round(tot.pres.nat.lw), "-", round(tot.pres.nat.hi) ,")")
paste0("Difference: ", round(diff), " Pg Carbon / year (95%-CI: ", round(diff.lw), "-", round(diff.hi) ,")")
### CACULATE TOTALT GLOBAL CONSUMPTION |||


### Carbon consumption map [MgC / km2 / year]
library(rworldmap)
library(maptools)
newmap <- getMap(resolution = "low")
newmap <- unionSpatialPolygons(newmap, rep(1, nrow(newmap)))
newmap <- spTransform(newmap, crs(current.consumption.map))
newmap <- fortify(newmap)

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
### Carbon consumption map [MgC / km2 / year] |||

# Load Modis data of NPP
# Modis data are based on leaf area and solar energy input
# It is total NPP, and before _any_ consumption 
# (other than what has been subtracted for respiration)
npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif") # [gC / m2 / yr]
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA
npp[] <- npp[] * 1000^2 / 10^6 # Mg Carbon / km2 / yr


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

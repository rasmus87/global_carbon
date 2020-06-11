# Run after 3.1_map.consumption.R

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

# Continent consumption [MgC / yr /km2]
continent.consumption <- continents %>% 
  left_join(current.consumption.map.df, by = c("x", "y")) %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y")) %>% 
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(period = fct_relevel(period, "Present Natural"))

ggplot(continent.consumption, aes(continent, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "")
continent.consumption %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm = T))

# Continent consumption of NPP [%]
continent.npp.consumption <- continents %>% 
  left_join(npp.use, by = c("x", "y")) %>% 
  na.omit()
cont.npp.use.plot <- ggplot(continent.npp.consumption, aes(continent, value, fill = time)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "")
# NPP (%) use by continent
m <- glm(value ~ continent + 0, data = continent.npp.consumption)
summary(m)

# Global consumption of NPP [%]
glob.npp.use.plot <- ggplot(continent.npp.consumption, aes(time, value, fill = time)) +
  geom_boxplot() +
  theme_bw() +
  ylab("") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.y = element_blank())   
m <- glm(value ~ time + 0, data = continent.npp.consumption)
summary(m)

continent.npp.consumption %>% group_by(time) %>% summarise(median(value, na.rm= T))
continent.npp.consumption %>% group_by(time, continent) %>% summarise(median(value, na.rm= T))
continent.npp.consumption %>% group_by(time) %>% summarise(quantile(value, .025, na.rm= T))
continent.npp.consumption %>% group_by(time) %>% summarise(quantile(value, .975, na.rm= T))
continent.npp.consumption %>% group_by(time) %>% summarise(mean(value, na.rm= T))
continent.npp.consumption %>% group_by(time) %>% summarise(sd(value, na.rm= T))

# Duplicate the dataset for boxplot one for continents and one for global
continent.npp.consumption$overgroup <- "Continent"
global.temp <- continent.npp.consumption
global.temp$overgroup <- "Global"
global.temp$continent <- "Global"
npp.consumption.plotting.df <- rbind(continent.npp.consumption, global.temp)
# Normal boxplot
ggplot(npp.consumption.plotting.df, aes(continent, value, fill = time)) +
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
# Violin-plot (with same data)
ggplot(npp.consumption.plotting.df, aes(continent, value, fill = time)) +
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
ggsave("./output/fig3_npp.use.png", width = 20, height = 10, units = "cm")


######### Consumption by realm and biome >>>
# Load LTW realms
base.map <- raster("builds/base_map.tif")
ltw.realm <- base.map
# old:
# ltw.biome[] <-  raster("data/ltw_realm.tif")[]
# New (C:\Files\GIS\ltw):
# It is based on ArcGIS polygons to raster
# (which doesn't accept Phylacines stupid recatangular cells)
# Fix alignment only ~4000 m off
# New one is not filtered for extreme NPP values
ltw.realm[] <- raster("data/ltw_v2geo_realm.tif")[]
ltw.realm <- as(ltw.realm, "SpatialPixelsDataFrame")
ltw.realm <- as_data_frame(ltw.realm)
colnames(ltw.realm) <- c("value", "x", "y")
names <- read_csv("data/ltw_realm_names.csv")
ltw.realm <- left_join(ltw.realm, names, by = c("value" = "ltw_realm"))
ggplot(ltw.realm, aes(x = x, y = y, fill = name)) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "LTW Realm", na.value = "white", discrete = T) +
  theme_map() +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Merge consumption by realm
consumption.realm <- ltw.realm %>% 
  transmute(x,y, ltw.realm = as.factor(name)) %>% 
  left_join(current.consumption.map.df, by = c("x", "y")) %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y")) %>% 
  gather(key = "time", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(time = fct_recode(time, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(time = fct_relevel(time, "Present Natural"))

# Merge consumption by realm
ggplot(consumption.realm, aes(ltw.realm, NPP.consumption, fill = time)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Last of the wild - Realm") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))

# Load LTW biomes
base.map <- raster("builds/base_map.tif")
ltw.biome <- base.map
# old:
# ltw.biome[] <- raster("data/ltw_biome.tif")[]
# New (C:\Files\GIS\ltw):
# It is based on ArcGIS polygons to raster
# (which doesn't accept Phylacines stupid recatangular cells)
# Fix alignment only ~4000 m off
# New one is not filtered for extreme NPP values
ltw.biome[] <- raster("data/ltw_v2geo_biome.tif")[]
# Na values are 128
ltw.biome[ltw.biome[] == 128] <- NA
ltw.biome <- as(ltw.biome, "SpatialPixelsDataFrame")
ltw.biome <- as_data_frame(ltw.biome)
colnames(ltw.biome) <- c("value", "x", "y")
names <- read_csv("data/ltw_biome_names.csv")
ltw.biome <- left_join(ltw.biome, names, by = c("value" = "ltw_biome"))
ggplot(ltw.biome, aes(x = x, y = y, fill = name)) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "LTW Biome", na.value = "white", discrete = T) +
  theme_map() +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

# Merge consumption by biome
consumption.biome <- ltw.biome %>% 
  transmute(x, y, ltw.biome = as_factor(name)) %>% 
  left_join(current.consumption.map.df, by = c("x", "y")) %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y")) %>% 
  gather(key = "time", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(time = fct_recode(time, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(time = fct_relevel(time, "Present Natural"))

# Plot consumption by biome
# ggplot(consumption.biome, aes(ltw.biome, NPP.consumption, fill = time)) +
#   geom_boxplot() +
#   theme_bw() +
#   ylab(expression(Consumption~(MgC/yr/km^2))) +
#   xlab("Last of the wild - Biome") +
#   scale_fill_brewer(type = "qual", name = "") +
#   theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))



current.consumption <- current.consumption.map.df %>% 
  left_join(present.natural.consumption.map.df, by = c("x", "y")) %>% 
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(period = fct_relevel(period, "Present Natural"))

ggplot(current.consumption, aes(period, NPP.consumption, fill = period)) +
  geom_boxplot() +
  theme_bw() +
  ylab(expression(Consumption~(MgC/yr/km^2))) +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8))
m <- glm(NPP.consumption ~ period, data = current.consumption)
summary(m)
current.consumption %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))
######### Consumption by realm and biome |||


######### NPP use by realm and biome >>>
current.npp.use.org.spdf <- as(current.npp.use.org, "SpatialPixelsDataFrame")
current.npp.use.org.df <- as_data_frame(current.npp.use.org.spdf)
colnames(current.npp.use.org.df) <- c("value", "x", "y")
present.natural.npp.use.org.spdf <- as(present.natural.npp.use.org, "SpatialPixelsDataFrame")
present.natural.npp.use.org.df <- as_data_frame(present.natural.npp.use.org.spdf)
colnames(present.natural.npp.use.org.df) <- c("value", "x", "y")

npp.consumption.realm <- ltw.realm %>% 
  transmute(x,y, ltw = as.factor(name)) %>% 
  left_join(current.npp.use.org.df, by = c("x", "y")) %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y")) %>% 
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(period = fct_relevel(period, "Present Natural"))

# Testplot
# ggplot(npp.consumption.realm, aes(ltw, NPP.consumption, fill = period)) +
#   geom_boxplot(notch = TRUE) +
#   theme_bw() +
#   ylab("NPP use (%)") +
#   xlab(NULL) +
#   scale_fill_brewer(type = "qual", name = "") +
#   coord_cartesian(ylim = c(0, 100)) +
#   labs(subtitle = "a) Realm") +
#   theme(plot.subtitle = element_text(face = "bold"))

npp.consumption.realm %>% group_by(period) %>% summarise(min = min(NPP.consumption, na.rm= T),
                                                         q025 = quantile(NPP.consumption, .025, na.rm= T),
                                                         med = median(NPP.consumption, .025, na.rm= T),
                                                         q975 = quantile(NPP.consumption, .975, na.rm= T), 
                                                         max = max(NPP.consumption, na.rm= T),
                                                         n = n(),
                                                         mean = mean(NPP.consumption, na.rm= T),
                                                         sd = sd(NPP.consumption, na.rm= T))
m <- glm(NPP.consumption ~ period, data = npp.consumption.realm)
summary(m)

npp.consumption.biome <- ltw.biome %>% 
  transmute(x,y, ltw = as.factor(name)) %>% 
  left_join(current.npp.use.org.df, by = c("x", "y")) %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y")) %>% 
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(period = fct_relevel(period, "Present Natural"))

# Testplot
# ggplot(npp.consumption.biome, aes(ltw, NPP.consumption, fill = period)) +
#   geom_boxplot(notch = FALSE) +
#   theme_bw() +
#   ylab("NPP use (%)") +
#   xlab(NULL) +
#   scale_fill_brewer(type = "qual", name = "") +
#   coord_cartesian(ylim = c(0, 100)) +
#   theme(axis.text.x=element_text(angle=30, hjust=1)) +
#   labs(subtitle = "b) Biome") +
#   theme(plot.subtitle = element_text(face = "bold"))


# Merge LTW NPP consumption for realm and biome and plot
npp.consumption.realm$group <- "Realm"
npp.consumption.biome$group <- "Biome"
npp.consumption.ltw <- bind_rows(npp.consumption.realm, npp.consumption.biome)
npp.consumption.ltw$group <- as_factor(npp.consumption.ltw$group)
npp.consumption.ltw <- na.omit(npp.consumption.ltw)
ggplot(npp.consumption.ltw, aes(ltw, NPP.consumption, fill = period)) +
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
ggsave("./output/fig4_npp_use_realm_biome.png", width = 25, height = 18, units = "cm")

# Test without limits
ggplot(npp.consumption.biome, aes(ltw, NPP.consumption, fill = period)) +
  geom_boxplot(notch = FALSE) +
  theme_bw() +
  ylab("NPP use (%)") +
  xlab("Continent") +
  scale_fill_brewer(type = "qual", name = "") +
  scale_y_log10() +
  theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=0.8))




######### NPP use by continent for LTW >>>

npp.consumption.ltw.continent <- ltw.realm %>% 
  transmute(x,y, ltw = as.factor(name)) %>% 
  left_join(continents) %>% 
  left_join(current.npp.use.org.df, by = c("x", "y")) %>% 
  left_join(present.natural.npp.use.org.df, by = c("x", "y")) %>% 
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>% 
  mutate(period = fct_recode(period, "Present Natural" = "value.y", "Current" = "value.x")) %>% 
  mutate(period = fct_relevel(period, "Present Natural"))

glm(NPP.consumption ~ period, data = npp.consumption.ltw.continent)
npp.consumption.ltw.continent %>% group_by(period) %>% summarise(median(NPP.consumption, na.rm= T))
npp.consumption.ltw.continent %>% group_by(period) %>% summarise(quantile(NPP.consumption, .025, na.rm= T))
npp.consumption.ltw.continent %>% group_by(period) %>% summarise(quantile(NPP.consumption, .975, na.rm= T))
npp.consumption.ltw.continent %>% group_by(period) %>% summarise(mean(NPP.consumption, na.rm= T))
npp.consumption.ltw.continent %>% group_by(period) %>% summarise(sd(NPP.consumption, na.rm= T))

# Duplicate for plotting
npp.consumption.ltw.continent$overgroup <- "Continent"
global.npp.temp <- npp.consumption.ltw.continent
global.npp.temp$overgroup <- "Global"
global.npp.temp$continent <- "Global"
npp.consumption.ltw.continent.duplicated <- rbind(npp.consumption.ltw.continent, global.npp.temp)

ggplot(npp.consumption.ltw.continent.duplicated %>% filter(NPP.consumption < 100), aes(continent, NPP.consumption, fill = period)) +
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
ggsave("./output/fig_ltw_continent.png", width = 20, height = 10, units = "cm")


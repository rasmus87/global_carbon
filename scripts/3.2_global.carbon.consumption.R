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
# Load biomes
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

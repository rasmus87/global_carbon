base.map <- raster("builds/base_map.tif")
wwf.realm <- base.map
wwf.realm[] <- raster("data/wwf_terr_ecos_realm_raster.tif")[]
wwf.biome <- base.map
wwf.biome[] <- raster("data/wwf_terr_ecos_biome_raster.tif")[]

# na.cells <- which(is.na(current.consumption.map[]) | is.na(present.natural.consumption.map[]) | wwf.realm[] == 4 | wwf.biome[] %in% c(98, 99, 128) | is.na(wwf.realm[]) | is.na(wwf.biome[]))
na.cells <- which(wwf.realm[] == 4 | wwf.biome[] %in% c(98, 99, 128) | is.na(wwf.realm[]) | is.na(wwf.biome[]))

madagascar <- c(33345, 33346, 33347, 33348, 33349, 33350, 33351, 33352, 33353, 33705, 33706, 33707, 33708, 33709, 33710, 33711, 33712, 33713, 34065, 34066, 34067, 34068, 34069, 34070, 34071, 34072, 34073, 34425, 34426, 34427, 34428, 34429, 34430, 34431, 34432, 34433, 34785, 34786, 34787, 34788, 34789, 34790, 34791, 34792, 34793, 35145, 35146, 35147, 35148, 35149, 35150, 35151, 35152, 35153, 35505, 35506, 35507, 35508, 35509, 35510, 35511, 35512, 35513, 35865, 35866, 35867, 35868, 35869, 35870, 35871, 35872, 35873, 36225, 36226, 36227, 36228, 36229, 36230, 36231, 36232, 36233, 36585, 36586, 36587, 36588, 36589, 36590, 36591, 36592, 36593, 36945, 36946, 36947, 36948, 36949, 36950, 36951, 36952, 36953, 37305, 37306, 37307, 37308, 37309, 37310, 37311, 37312, 37313, 37665, 37666, 37667, 37668, 37669, 37670, 37671, 37672, 37673, 38025, 38026, 38027, 38028, 38029, 38030, 38031, 38032, 38033, 38385, 38386, 38387, 38388, 38389, 38390, 38391, 38392, 38393, 38745, 38746, 38747, 38748, 38749, 38750, 38751, 38752, 38753, 39105, 39106, 39107, 39108, 39109, 39110, 39111, 39112, 39113, 39465, 39466, 39467, 39468, 39469, 39470, 39471, 39472, 39473, 39825, 39826, 39827, 39828, 39829, 39830, 39831, 39832, 39833)


# Load WWF realms
wwf.realm[na.cells] <- NA
wwf.realm <- as(wwf.realm, "SpatialPixelsDataFrame")
wwf.realm <- as_data_frame(wwf.realm)
colnames(wwf.realm) <- c("value", "x", "y")
names <- read_csv("data/wwf_realm_names.csv")
wwf.realm <- left_join(wwf.realm, names, by = c("value" = "wwf_realm"))

ggplot(wwf.realm, aes(x = x, y = y, fill = name)) +
  geom_tile() +
  coord_equal(ylim = range(wwf.realm$y)) +
  scale_fill_viridis(name = "WWF Realm", na.value = "pink", discrete = T, option = "D") +
  theme_map() +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)


# Load WWF biomes
wwf.biome[na.cells] <- NA
wwf.biome <- as(wwf.biome, "SpatialPixelsDataFrame")
wwf.biome <- as_data_frame(wwf.biome)
colnames(wwf.biome) <- c("value", "x", "y")
names <- read_csv("data/wwf_biome_names.csv")
wwf.biome <- left_join(wwf.biome, names, by = c("value" = "wwf_biome"))

ggplot(wwf.biome, aes(x = x, y = y, fill = name)) +
  geom_tile() +
  coord_equal(ylim = range(wwf.biome$y)) +
  scale_fill_viridis(name = "WWF biome", na.value = "pink", discrete = T, option = "D") +
  theme_map() +
  theme(legend.background = element_blank()) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)



base.map <- raster("builds/base_map.tif")
wwf.realm <- base.map
wwf.realm[] <- raster("data/wwf_terr_ecos_realm_raster.tif")[]
wwf.biome <- base.map
wwf.biome[] <- raster("data/wwf_terr_ecos_biome_raster.tif")[]

na.cells <- which(wwf.realm[] %in% c(4, 7) | wwf.biome[] %in% c(98, 99, 128) | is.na(wwf.realm[]) | is.na(wwf.biome[]))
madagascar <- c(33345, 33346, 33347, 33348, 33349, 33350, 33351, 33352, 33353, 33705, 33706, 33707, 33708, 33709, 33710, 33711, 33712, 33713, 34065, 34066, 34067, 34068, 34069, 34070, 34071, 34072, 34073, 34425, 34426, 34427, 34428, 34429, 34430, 34431, 34432, 34433, 34785, 34786, 34787, 34788, 34789, 34790, 34791, 34792, 34793, 35145, 35146, 35147, 35148, 35149, 35150, 35151, 35152, 35153, 35505, 35506, 35507, 35508, 35509, 35510, 35511, 35512, 35513, 35865, 35866, 35867, 35868, 35869, 35870, 35871, 35872, 35873, 36225, 36226, 36227, 36228, 36229, 36230, 36231, 36232, 36233, 36585, 36586, 36587, 36588, 36589, 36590, 36591, 36592, 36593, 36945, 36946, 36947, 36948, 36949, 36950, 36951, 36952, 36953, 37305, 37306, 37307, 37308, 37309, 37310, 37311, 37312, 37313, 37665, 37666, 37667, 37668, 37669, 37670, 37671, 37672, 37673, 38025, 38026, 38027, 38028, 38029, 38030, 38031, 38032, 38033, 38385, 38386, 38387, 38388, 38389, 38390, 38391, 38392, 38393, 38745, 38746, 38747, 38748, 38749, 38750, 38751, 38752, 38753, 39105, 39106, 39107, 39108, 39109, 39110, 39111, 39112, 39113, 39465, 39466, 39467, 39468, 39469, 39470, 39471, 39472, 39473, 39825, 39826, 39827, 39828, 39829, 39830, 39831, 39832, 39833)
na.cells <- c(na.cells, madagascar)

wwf.realm[na.cells] <- NA
wwf.realm <- as(wwf.realm, "SpatialPixelsDataFrame")
wwf.realm <- as_data_frame(wwf.realm)
colnames(wwf.realm) <- c("value", "x", "y")
names <- read_csv("data/wwf_realm_names.csv")
wwf.realm <- left_join(wwf.realm, names, by = c("value" = "wwf_realm"))
wwf.realm <- wwf.realm %>% 
  transmute(x, y, realm = name)

wwf.biome[na.cells] <- NA
wwf.biome <- as(wwf.biome, "SpatialPixelsDataFrame")
wwf.biome <- as_data_frame(wwf.biome)
colnames(wwf.biome) <- c("value", "x", "y")
names <- read_csv("data/wwf_biome_names.csv")
wwf.biome <- left_join(wwf.biome, names, by = c("value" = "wwf_biome"))
wwf.biome <- wwf.biome %>% 
  transmute(x, y, biome = name)

eco.units <- left_join(wwf.realm, wwf.biome, by = c("x", "y"))

leave.out <- c("Tropical and Subtropical Coniferous Forests", "Flooded Grasslands and Savannas",
               "Montane Grasslands and Shrublands", "Tundra", "Deserts and Xeric Shrublands", "Mangroves")
eco.units <- eco.units %>%
  filter(!biome %in% leave.out)



eco.units <- eco.units %>% 
  mutate(realm = ifelse(realm %in% c("Nearctic", "Neotropic"), "New World", realm)) %>% 
  mutate(biome = ifelse(biome %in% c("Temperate Broadleaf and Mixed Forests", "Temperate Coniferous Forests"), "Temperate Forests", biome)) %>% 
  mutate(biome = ifelse(biome %in% c("Tropical and Subtropical Dry Broadleaf Forests", "Tropical and subtropical grasslands, savannas, and shrublands"), "Tropical and Subtropical Grasslands to Forests", biome)) %>% 
  mutate(eco.unit = paste(realm, biome, sep = " - "))

eco.units %>% count(eco.unit) %>% print(n = 100)

keep.units <- c(
"Afrotropic - Tropical and Subtropical Grasslands to Forests",
"Afrotropic - Tropical and Subtropical Moist Broadleaf Forests",
"Australasia - Mediterranean Forests, Woodlands, and Scrub",
"Australasia - Temperate Forests",
"Australasia - Temperate Grasslands, Savannas, and Shrublands",
"Australasia - Tropical and Subtropical Grasslands to Forests",
"Australasia - Tropical and Subtropical Moist Broadleaf Forests",
"Indomalaya - Tropical and Subtropical Grasslands to Forests",
"Indomalaya - Tropical and Subtropical Moist Broadleaf Forests",
"New World - Boreal Forests/Taiga",
"New World - Temperate Forests",
"New World - Temperate Grasslands, Savannas, and Shrublands",
"New World - Tropical and Subtropical Grasslands to Forests",
"New World - Tropical and Subtropical Moist Broadleaf Forests",
"Palearctic - Boreal Forests/Taiga",
"Palearctic - Mediterranean Forests, Woodlands, and Scrub",
"Palearctic - Temperate Forests",
"Palearctic - Temperate Grasslands, Savannas, and Shrublands")

eco.units <- eco.units %>% filter(eco.unit %in% keep.units)

ggplot(eco.units, aes(x = x, y = y, fill = eco.unit)) +
  geom_tile() +
  coord_equal(ylim = range(wwf.biome$y)) +
  scale_fill_viridis(name = "WWF biome", na.value = "pink", discrete = T, option = "D") +
  theme_map() +
  theme(legend.background = element_blank()) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

eco.units %>% count(eco.unit)

eco.units$biome <- str_replace(eco.units$biome, "Temperate Grasslands, Savannas, and Shrublands", "Temperate Grasslands to Shrublands")
eco.units$biome <- str_replace(eco.units$biome, "Mediterranean Forests, Woodlands, and Scrub", "Mediterranean Forests to Scrub")

theme_R <- function() {
  theme_bw() %+replace% 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
}

### Eco-unit consumption [MgC / yr /km2] ###
eco.unit.consumption <- eco.units %>%
  full_join(current.consumption.map.df, by = c("x", "y")) %>%
  full_join(present.natural.consumption.map.df, by = c("x", "y")) %>%
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>%
  mutate(period = fct_recode(period, "Present natural" = "value.y", "Current" = "value.x")) %>%
  mutate(period = fct_relevel(period, "Present natural")) %>% 
  mutate(time.x = NULL, time.y = NULL)

# Duplicate the dataset for boxplot one for continents and one for global
global.temp <- eco.unit.consumption
global.temp$biome <- "Global"
global.temp$realm <- "Global"
eco.unit.consumption <- eco.unit.consumption %>% filter(!is.na(realm))
eco.unit.consumption.plotting.df <- rbind(eco.unit.consumption, global.temp)
eco.unit.consumption.plotting.df <- eco.unit.consumption.plotting.df %>%
  mutate(realm = as_factor(realm),
         biome = as_factor(biome))

# Violin-plot
p3a <- ggplot(eco.unit.consumption.plotting.df, aes(realm, NPP.consumption, fill = period)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width") +
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.7), outlier.shape = NA, show.legend = FALSE) +
  theme_bw() +
  ylab(expression((a)~Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = c("Present natural" = "chartreuse3", "Current" = "#FFD18D"), name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none"
  ) 
# ggsave("./output/fig3a_consumption.png", width = 29, height = 15, units = "cm")

### Eco-unit consumption of NPP [%] ###
eco.unit.npp.consumption <- eco.units %>% 
  full_join(npp.use, by = c("x", "y"))

# Duplicate the dataset for boxplot one for continents and one for global
global.temp <- eco.unit.npp.consumption
global.temp$biome <- "Global"
global.temp$realm <- "Global"
eco.unit.npp.consumption <- eco.unit.npp.consumption %>% filter(!is.na(realm))
npp.consumption.plotting.df <- rbind(eco.unit.npp.consumption, global.temp)
npp.consumption.plotting.df <- npp.consumption.plotting.df %>%
  mutate(realm = as_factor(realm),
         biome = as_factor(biome))

# Violin-plot
p3b <- ggplot(npp.consumption.plotting.df, aes(realm, value, fill = time)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width") +
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.7), outlier.shape = NA, show.legend = FALSE) +
  theme_bw() +
  ylab("(b) Fraction of NPP consumed (%)") +
  xlab(NULL) +
  scale_fill_manual(values = c("Present natural" = "chartreuse3", "Current" = "#FFD18D"), name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none"
  )
# ggsave("./output/fig3b_npp.use.png", width = 29, height = 15, units = "cm")




# Last of the wild:
base.map <- raster("builds/base_map.tif")

ltw.realm <- base.map
ltw.realm[] <- raster("data/ltw_v2geo_realm.tif")[]
ltw.realm[is.na(ltw.realm[])] <- 0
ltw.realm[which(ltw.realm[] > 0)] <- 1

ltw.biome <- base.map
ltw.biome[] <- raster("data/ltw_v2geo_biome.tif")[]
ltw.biome[ltw.biome[] == 128] <- 0
ltw.biome[which(ltw.biome[] > 0)] <- 1

ltw <- base.map

ltw[ltw.realm[] + ltw.biome[] == 1] <- 1
plot(ltw)

sum(ltw.realm[] < ltw.biome[])
sum(ltw.realm[] > ltw.biome[])

ltw[ltw.realm[] | ltw.biome[]] <- 1

ltw <- as(ltw, "SpatialPixelsDataFrame")
ltw <- as_data_frame(ltw)
colnames(ltw) <- c("value", "x", "y")

# Merge consumption by biome
ltw.eco.unit.consumption <-  ltw %>% dplyr::select(x, y) %>% 
  left_join(eco.units, by = c("x", "y")) %>% 
  left_join(current.consumption.map.df, by = c("x", "y")) %>%
  left_join(present.natural.consumption.map.df, by = c("x", "y")) %>%
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>%
  mutate(period = fct_recode(period, "Present natural" = "value.y", "Current" = "value.x")) %>%
  mutate(period = fct_relevel(period, "Present natural")) %>% 
  mutate(time.x = NULL, time.y = NULL)

ggplot(ltw.eco.unit.consumption, aes(x = x, y = y, fill = eco.unit)) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "LTW Biome", na.value = "white", discrete = T) +
  theme_map() +
  theme(legend.position = "right") + 
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

ltw.eco.unit.consumption <- ltw %>% dplyr::select(x, y) %>% 
  left_join(eco.units, by = c("x", "y")) %>% 
  left_join(npp.use, by = c("x", "y")) %>% 
  na.omit()

# Duplicate the dataset for boxplot one for continents and one for global
global.temp <- eco.unit.npp.consumption
global.temp$biome <- "Global"
global.temp$realm <- "Global"
ltw.eco.unit.consumption.plotting.df <- rbind(ltw.eco.unit.consumption, global.temp)
ltw.eco.unit.consumption.plotting.df <- ltw.eco.unit.consumption.plotting.df %>%
  mutate(realm = fct_relevel(realm, levels = levels(npp.consumption.plotting.df$realm)),
         biome = fct_relevel(biome, levels = levels(npp.consumption.plotting.df$biome)))

# Add NA's to missing categories
ltw.eco.unit.consumption.plotting.df <- npp.consumption.plotting.df %>% 
  filter(realm != "Global") %>% 
  count(realm, biome, time) %>%
  .[-4] %>% 
  bind_rows(ltw.eco.unit.consumption.plotting.df)

ltw.eco.unit.consumption.plotting.df <- ltw.eco.unit.consumption.plotting.df %>% 
  bind_rows(npp.consumption.plotting.df %>% 
              filter(realm != "Global") %>% 
              count(realm, biome, time) %>%
              .[-4])

p3c <- ggplot(ltw.eco.unit.consumption.plotting.df, aes(realm, value, fill = time)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width") +
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.7), outlier.shape = NA, show.legend = FALSE) +
  theme_bw() +
  ylab("(c) Fraction of NPP consumed in LTW (%)") +
  xlab(NULL) +
  scale_fill_manual(values = c("Present natural" = "chartreuse3", "Current" = "#FFD18D"), name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "bottom"
  )


library(cowplot)

p <- plot_grid(p3a, p3b, p3c, nrow = 3)
ggsave("./output/fig3_violins200.png", p, width = 29, height = 40, units = "cm")

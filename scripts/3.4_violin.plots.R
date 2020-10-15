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
wwf.realm <- as_tibble(wwf.realm)
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
wwf.biome <- as_tibble(wwf.biome)
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
na.cells <- c(na.cells, remove.areas)


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
  mutate(realm = ifelse(realm %in% c("Nearctic", "Neotropic"), "Americas", realm)) %>% 
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
"Americas - Boreal Forests/Taiga",
"Americas - Temperate Forests",
"Americas - Temperate Grasslands, Savannas, and Shrublands",
"Americas - Tropical and Subtropical Grasslands to Forests",
"Americas - Tropical and Subtropical Moist Broadleaf Forests",
"Palearctic - Boreal Forests/Taiga",
"Palearctic - Mediterranean Forests, Woodlands, and Scrub",
"Palearctic - Temperate Forests",
"Palearctic - Temperate Grasslands, Savannas, and Shrublands")

eco.units <- eco.units %>% filter(eco.unit %in% keep.units)
eco.units %>% count(eco.unit)

eco.units$biome <- str_replace(eco.units$biome, "Temperate Grasslands, Savannas, and Shrublands", "Temperate Grasslands to Shrublands")
eco.units$biome <- str_replace(eco.units$biome, "Mediterranean Forests, Woodlands, and Scrub", "Mediterranean Forests to Scrub")

# cols <- colorspace::qualitative_hcl(palette = "Dark 3", n = 5)
# breaks = c("Americas", "Afrotropic", "Palearctic", "Indomalaya", "Australasia")
# eco.units.plot <- ggplot(eco.units, aes(x = x, y = y, fill = realm)) +
#   facet_wrap(vars(biome), nrow = 3) +
#   geom_tile() +
#   coord_equal(ylim = range(eco.units$y), xlim = range(eco.units$x)) +
#   scale_fill_manual(name = "Realm", na.value = "pink", values = cols, breaks = breaks) +
#   # colorspace::scale_fill_discrete_qualitative(name = "WWF Realm", palette = "Dark 3", n = 5) +
#   ggthemes::theme_map() +
#   theme(legend.position = "bottom", legend.justification = NULL) +
#   geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)
# ggsave("./output/fig_ecoregions.png", eco.units.plot, width = 183, height = 137, units = "mm", dpi = 600)


### Eco-unit consumption [MgC / yr /km2] ###
eco.unit.consumption <- eco.units %>%
  full_join(current.consumption.df, by = c("x", "y")) %>%
  full_join(present.natural.consumption.df, by = c("x", "y")) %>%
  gather(key = "period", value = "NPP.consumption", value.x, value.y) %>%
  mutate(period = fct_recode(period, "Present natural" = "value.y", "Current" = "value.x")) %>%
  mutate(period = fct_relevel(period, "Present natural")) %>% 
  mutate(period.x = NULL, period.y = NULL)

# Duplicate the dataset for boxplot one for global and one for eco units
global.consumption <- eco.unit.consumption
global.consumption$biome <- "Global"
global.consumption$realm <- "Global"

# eco.unit.consumption <- eco.unit.consumption %>% filter(!is.na(realm))

period.colors <- c("Present natural" = "#b2df8a", "Current" = "#a6cee3")

# Violin-plot
p4a <- ggplot(eco.unit.consumption %>% filter(!is.na(realm)), aes(realm, NPP.consumption, fill = period)) +
  facet_grid(cols = vars(biome), scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_bw() +
  ylab(expression((a)~Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
p4a



### Eco-unit consumption of NPP [%] ###
eco.unit.npp.consumption <- eco.units %>% 
  full_join(npp.use, by = c("x", "y"))

# Merge consumption by biome
ltw.eco.unit.consumption <- ltw %>% 
  left_join(eco.units, by = c("x", "y"))


# Duplicate the dataset for boxplot one for global and one for eco units
global.npp.consumption <- eco.unit.npp.consumption
global.npp.consumption$biome <- "Global"
global.npp.consumption$realm <- "Global"

eco.unit.npp.consumption <- eco.unit.npp.consumption %>% filter(!is.na(realm))

# Violin-plot
p4b <- ggplot(eco.unit.npp.consumption %>% filter(!is.na(realm)), aes(realm, value, fill = period)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_bw() +
  ylab("(b) Fraction of NPP consumed (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )






ggplot(ltw.eco.unit.consumption, aes(x = x, y = y, fill = eco.unit)) +
  geom_tile() +
  coord_equal(ylim = range(cu.npp.use$y)) +
  scale_fill_viridis(name = "LTW Biome", na.value = "white", discrete = T) +
  theme_map() +
  theme(legend.position = "right") +
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)


# Duplicate the dataset for boxplot one for global and one for eco units
ltw.eco.unit.consumption <- ltw.eco.unit.consumption %>% filter(!is.na(value))
global.ltw.consumption <- ltw.eco.unit.consumption
global.ltw.consumption$biome <- "Global"
global.ltw.consumption$realm <- "Global"

ltw.eco.unit.consumption <- ltw.eco.unit.consumption %>%
  filter(!is.na(realm)) %>% 
  mutate(realm = fct_relevel(realm, levels = levels(ltw.eco.unit.consumption$realm)),
         biome = fct_relevel(biome, levels = levels(ltw.eco.unit.consumption$biome)))

# Add NA's to missing categories
ltw.eco.unit.consumption <- ltw.eco.unit.consumption %>%
  bind_rows(eco.unit.npp.consumption %>%
              count(realm, biome, period) %>%
              .[-4])

p4c <- ggplot(ltw.eco.unit.consumption, aes(realm, value, fill = period)) +
  facet_grid(. ~ biome, scale = "free", space = "free") +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7),
    show.legend = FALSE) +
  theme_bw() +
  ylab("(c) Fraction of NPP consumed in LTW (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
p4c

p <- plot_grid(p4a, p4b, p4c, nrow = 3)

if(full) {
  ggsave("./output/fig4_eco_units_violins_full.png", p, width = 183, height = 220, units = "mm", dpi = 600, scale = 1.5)
} else {
  ggsave("./output/fig4_eco_units_violins200.png", p, width = 183, height = 220, units = "mm", dpi = 600, scale = 1.5)
}



### Global plots
p3a <- ggplot(global.consumption, aes(period, NPP.consumption, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab(expression((a)~Consumption~(MgC/yr/km^2))) +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p3b <- ggplot(global.npp.consumption %>% filter(!is.na(period)), aes(period, value, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab("(b) Fraction of NPP consumed (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p3c <- ggplot(global.ltw.consumption, aes(period, value, fill = period)) +
  geom_violin(width = 0.7, scale = "width", linetype = "blank") +
  geom_boxplot(
    aes(group = interaction(realm, period)), 
    width = 0.10,
    col = "black",
    position = position_dodge(width = 0.7),
    outlier.shape = 20, 
    outlier.size = 0.5,
    show.legend = FALSE, 
    fill = "black",
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    col = "white",
    size = 1,
    shape = 20,
    position = position_dodge(width = 0.7)) +
  theme_R() +
  ylab("(c) Fraction of NPP consumed in LTW (%)") +
  xlab(NULL) +
  scale_fill_manual(values = period.colors, name = "Period") +
  theme(
    strip.text.x = element_text(size = 5.4),
    axis.text.x = element_text(angle = 30, vjust = .8, hjust = .8),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

p3 <- plot_grid(p3a, p3b, p3c, nrow = 1)

if(full) {
  ggsave("./output/fig3_global_violins_full.png", p3, width = 89, height = 89, units = "mm", dpi = 600, scale = 1.5)
} else {
  ggsave("./output/fig3_global_violins200.png", p3, width = 89, height = 89, units = "mm", dpi = 600, scale = 1.5)
}

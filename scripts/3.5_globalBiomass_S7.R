library(raster)
load("builds/current.maps.filtered.edge.lim.RData")
load("builds/present.natural.maps.filtered.edge.lim.RData")

df <- read_csv("builds/data.csv", col_types = cols())

mass.Pg <- df$Mass.g * 10^-15 # Pg (or Gt)
density <- 10^df$log10density # km^-2

mammal.biomass.area <- density * mass.Pg * 10^15 / 1000 # Kg / km2

cu.mass <- current.maps.edge.lim * mammal.biomass.area
cu.mass <- colSums(cu.mass)

pn.mass <- present.natural.maps.edge.lim * mammal.biomass.area
pn.mass <- colSums(pn.mass)


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

cu.mass.map <- base.map
cu.mass.map[] <- cu.mass
cu.mass.map.spdf <- as(cu.mass.map, "SpatialPixelsDataFrame") # [MgC / km2 / year]
cu.mass.map.spdf.df <- as_tibble(cu.mass.map.spdf)
colnames(cu.mass.map.spdf.df) <- c("value", "x", "y")
cu.mass.map.spdf.df <- cu.mass.map.spdf.df %>% mutate(time = "Current")

pn.mass.map <- base.map
pn.mass.map[] <- pn.mass
pn.mass.map.spdf <- as(pn.mass.map, "SpatialPixelsDataFrame") # [MgC / km2 / year]
pn.mass.map.spdf.df <- as_tibble(pn.mass.map.spdf)
colnames(pn.mass.map.spdf.df) <- c("value", "x", "y")
pn.mass.map.spdf.df <- pn.mass.map.spdf.df %>% mutate(time = "Present-natural")

mass.map.df <- bind_rows(cu.mass.map.spdf.df, pn.mass.map.spdf.df)

mass <- left_join(continents, mass.map.df)


library(rworldmap)
library(maptools)
library(viridis)
library(ggthemes)
base.map <- raster("builds/base_map.tif")
newmap <- getMap(resolution = "low")
newmap <- unionSpatialPolygons(newmap, rep(1, nrow(newmap)))
newmap <- spTransform(newmap, crs(base.map))
newmap <- fortify(newmap)


p1a <- ggplot(mass %>% filter(time == "Current"), aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(mass$y)) +
  scale_fill_viridis(name = Mass~(Kg/km^2),
                     na.value = "white") +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

p1b <- ggplot(mass %>% filter(time == "Current"), aes(value, col = continent)) +
  geom_density() +
  theme_bw() +
  #coord_cartesian(xlim = c(0, 30000)) +
  xlab(Mass~(Kg/km^2)) + 
  facet_grid(rows = vars(time), scales = "free_y")

library(cowplot)

plot_grid(p1a, p1b, nrow = 2)



p2a <- ggplot(mass %>% filter(time == "Present-natural"), aes(x = x, y = y, fill = value)) +
  facet_grid(time ~ .) +
  geom_tile() +
  coord_equal(ylim = range(mass$y)) +
  scale_fill_viridis(name = Mass~(Kg/km^2),
                     na.value = "white") +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

p2b <- ggplot(mass %>% filter(time == "Present-natural"), aes(value, col = continent)) +
  geom_density() +
  theme_bw() +
  #coord_cartesian(xlim = c(0, 30000)) +
  xlab(Mass~(Kg/km^2)) + 
  facet_grid(rows = vars(time), scales = "free_y")

plot_grid(p1a, p2a, p1b, p2b, ncol = 2)

p <- plot_grid(p1a, p1b, p2a, p2b, nrow = 4)
ggsave("./output/figS7_GlobalBiomass.png", p, width = 20, height = 25, units = "cm")

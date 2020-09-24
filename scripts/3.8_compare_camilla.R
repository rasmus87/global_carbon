# Compare our results to Camilla

library(sf)

camilla <- readxl::read_xlsx("data/camilla_supp.xlsx")
camilla.coords <- SpatialPoints(coords = camilla[c("Long", "Lat")], 
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

camilla.coords <- spTransform(camilla.coords, crs(base.map))
camilla.coords.df <- camilla.coords %>% as_tibble()
camilla <- bind_cols(camilla %>% dplyr::select(-Long, -Lat), camilla.coords.df)

ggplot(camilla, aes(x = Long, y = Lat, col = TotalHerbivoreBiomass_kgkm2)) +
  geom_point() +
  coord_equal() +
  scale_color_viridis(name = Mass~(Kg/km^2),
                     na.value = "white") +
  scale_fill_viridis(name = Mass~(Kg/km^2),
                     na.value = "white") +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_polygon(data = newmap, aes(x = long, y = lat, group = group), inherit.aes = F, col = "black", fill = "NA", lwd = .25)

df <- read_csv("builds/data.csv", col_types = cols())
mass.Pg <- df$Mass.g * 10^-15 # Pg (or Gt)
density <- 10^df$log10density # km^-2
herbivore <- df$Diet.Plant > 50
mammal.biomass.area <- density * herbivore * mass.Pg * 10^15 / 1000 # Kg / km2

cu.mass <- current.maps.edge.lim * mammal.biomass.area
cu.mass <- colSums(cu.mass)
cu.mass[remove.areas] <- NA
our.mass <- cu.mass.r[cellFromXY(cu.mass.r, camilla.coords)]


camilla$our.mass <- our.mass

ggplot(camilla, aes(our.mass, TotalHerbivoreBiomass_kgkm2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") + 
  geom_abline(slope = 1, lty = "dashed") +
  theme_R() +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
  annotate("text", x = 29701, y = 31226, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1)

ggsave("Output/S8_Compare_fløjgaard.jpg", width = 20, height = 13, units = "cm")


camilla %>% arrange(desc(TotalHerbivoreBiomass_kgkm2)) %>% dplyr::select(1:6, 14)

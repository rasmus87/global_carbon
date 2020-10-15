# Compare our results to Camilla

library(sf)
library(raster)
library(rworldmap)
library(maptools)
base.map <- raster("builds/base_map.tif")
newmap <- getMap(resolution = "low")
newmap <- unionSpatialPolygons(newmap, rep(1, nrow(newmap)))
newmap <- spTransform(newmap, crs(base.map))
newmap <- fortify(newmap)

current.maps <- readRDS("builds/current.maps.filtered.edge.lim.rds")
present.natural.maps <- readRDS("builds/present.natural.maps.filtered.edge.lim.rds")

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

cells <- cellFromXY(base.map, camilla.coords)

cu.mass <- current.maps * mammal.biomass.area
cu.mass <- colSums(cu.mass)
cu.mass[remove.areas] <- NA
cu.mass <- cu.mass[cells]

pn.mass <- present.natural.maps * mammal.biomass.area
pn.mass <- colSums(pn.mass)
pn.mass[remove.areas] <- NA
pn.mass <- pn.mass[cells]

realms <- tibble(realm = raster("data/wwf_terr_ecos_realm_raster.tif")[cells])
names <- read_csv("data/wwf_realm_names.csv")
realms <- left_join(realms, names, by = c("realm" = "wwf_realm"))
realms <- realms %>% mutate(name = ifelse(name %in% c("Nearctic", "Neotropic"), "Americas", name))

camilla$cu.mass <- cu.mass
camilla$pn.mass <- pn.mass
camilla$cell <- cells
camilla$areaID <- paste0(camilla$Name_Ecosystem_new, camilla$cell, sep = "_")
camilla$realm <- realms$name

camilla <- camilla %>% 
  filter(!is.na(cu.mass))

# ggplot(camilla, aes(cu.mass, TotalHerbivoreBiomass_kgkm2)) +
#   geom_point() +
#   geom_smooth(method = "lm", col = "black") + 
#   geom_abline(slope = 1, lty = "dashed") +
#   theme_R() +
#   ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
#   xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
#   annotate("text", x = 27911, y = 31226, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1) +
#   annotate("text", x = 1013, y = 11959, label = expression(" " %<-% "Oostvaardersplassen The Netherlands"), hjust = 0)
# 
# # ggsave("Output/Compare_fløjgaard.jpg", width = 183, height = 112, units = "mm", dpi = 600, scale = 1)


camilla %>% arrange(desc(TotalHerbivoreBiomass_kgkm2)) %>% dplyr::select(1:5, 14:15) %>% print(n=30)


camilla.summary = camilla %>%
  group_by(areaID) %>% 
  summarise(Mass = median(TotalHerbivoreBiomass_kgkm2),
            Mass.lw = min(TotalHerbivoreBiomass_kgkm2),
            Mass.hi = max(TotalHerbivoreBiomass_kgkm2),
            cu.mass = cu.mass[1],
            pn.mass = pn.mass[1],
            n = n(),
            realm = realm[1],
            Megafauna = max(Megafauna, na.rm = T)) %>%
  filter(n > 1)

# ggplot(camilla.summary, aes(cu.mass, Mass)) +
#   geom_abline(slope = 1, lty = "dashed") +
#   geom_point(data = camilla %>% filter(!Name_Ecosystem_new %in% camilla.summary$Name_Ecosystem_new), aes(cu.mass, TotalHerbivoreBiomass_kgkm2), col = "black") +
#   geom_point(data = camilla %>% filter(Name_Ecosystem_new %in% camilla.summary$Name_Ecosystem_new), aes(cu.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
#   geom_point() +
#   theme_R() +
#   coord_equal() +
#   ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
#   xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
#   geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi))

cols <- colorspace::qualitative_hcl(palette = "Dark 3", n = 5)
breaks = c("Americas", "Afrotropic", "Palearctic", "Indomalaya", "Australasia")
c <- ggplot(camilla.summary, aes(cu.mass, Mass, col = realm)) +
  geom_abline(slope = 1, lty = "dashed") +
  geom_point(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(cu.mass, TotalHerbivoreBiomass_kgkm2, col = realm)) +
  geom_point(data = camilla %>% filter(areaID %in% camilla.summary$areaID), aes(cu.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
  geom_point(shape = 5) +
  theme_R() +
  coord_equal(xlim = c(-500, 32000), ylim = c(-500, 32000), expand = FALSE) +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
  geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi)) +
  annotate("text", x = 27911, y = 29000, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1) +
  # annotate("text", x = 1013, y = 11959, label = expression(" " %<-% "Oostvaardersplassen the Netherlands"), hjust = 0) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks) +
  geom_path(data = tibble(x = c(0, 11000, 11000), y = c(16000, 16000, 0)), aes(x, y, col = NULL, shape = NULL), col = "black", lty = "dotted")

# ggsave("Output/Compare_fløjgaard.jpg", width = 170, height = 135, units = "mm", dpi = 600, scale = 1)



c1 <- ggplot(camilla.summary, aes(cu.mass, Mass, col = realm)) +
  geom_abline(slope = 1, lty = "dashed") +
  geom_point(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(cu.mass, TotalHerbivoreBiomass_kgkm2, col = realm)) +
  geom_point(data = camilla %>% filter(areaID %in% camilla.summary$areaID), aes(cu.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
  geom_point(shape = 5) +
  theme_R() +
  coord_equal(xlim = c(-500, 11000), ylim = c(-500, 16000), expand = FALSE) +
  scale_x_continuous(minor_breaks = seq(0, 11000, 2500), breaks = seq(0, 11000, 5000)) +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
  geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi)) +
  # annotate("text", x = 27911, y = 29000, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1) +
  # annotate("text", x = 1013, y = 11959, label = expression(" " %<-% "Oostvaardersplassen the Netherlands"), hjust = 0) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks)

library(cowplot)
p <- cowplot::plot_grid(c + theme(legend.position="none"),
                   c1 + theme(legend.position="none"),
                   align = 'none',
                   labels = c("a", "b"),
                   rel_widths = c(1, .90)
)

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend_b <- get_legend(c + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .05))
p

ggsave("Output/Compare_fløjgaard_test.jpg", width = 183, height = 100, units = "mm", dpi = 600, scale = 1.45)


ggplot(camilla.summary, aes(pn.mass, Mass, col = realm)) +
  geom_abline(slope = 1, lty = "dashed") +
  geom_point(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(pn.mass, TotalHerbivoreBiomass_kgkm2, col = realm)) +
  geom_point(data = camilla %>% filter(areaID %in% camilla.summary$areaID), aes(pn.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
  geom_point(shape = 5) +
  theme_R() +
  # coord_equal(xlim = c(-500, 11000), ylim = c(-500, 16000), expand = FALSE) +
  # scale_x_continuous(minor_breaks = seq(0, 11000, 2500), breaks = seq(0, 11000, 5000)) +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (Present-natural maps) (This paper) [" ~ kg/km^2 ~ "]")) +
  geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi)) +
  annotate("text", x = 27911, y = 29000, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1) +
  # annotate("text", x = 12331, y = 11959, label = expression(" " %<-% "Oostvaardersplassen the Netherlands"), hjust = 0) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks)

ggsave("Output/Compare_fløjgaard_PN.jpg", width = 183, height = 100, units = "mm", dpi = 600, scale = 1.45)

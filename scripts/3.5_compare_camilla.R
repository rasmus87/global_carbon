# Compare our results to the densities found in Fløjgaard et al.
# 28/07-2021 Rasmus Ø Pedersen


# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)

# Create ggplot theme
theme_R <- function() {
  theme_bw() %+replace% 
    theme(panel.border = element_blank(),
          axis.line = element_line(colour = "black"))
}


# Load traits
df <- read_csv("builds/data.csv", col_types = cols())

# Load world map
world.map <- giscoR::gisco_get_countries() %>%
  st_union() %>% 
  st_transform(st_crs(base.map))

# Base map
base.map <- raster("builds/base_map.tif")

# Load Fløjgaards data
# camilla.old <- readxl::read_xlsx("data/camilla_supp.xlsx") %>% 
#   st_as_sf(coords = c("Long", "Lat"),
#            crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
#   st_transform(st_crs(base.map))
camilla <- readxl::read_xlsx("data/camilla_supp_new.xlsx") %>% 
  st_as_sf(coords = c("Long", "Lat"),
           crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  st_transform(st_crs(base.map))

# Plot the sites
ggplot(camilla, aes(col = TotalHerbivoreBiomass_kgkm2)) +
  geom_sf() +
  scale_color_viridis(name = Mass~(kg/km^2),
                     na.value = "white") +
  scale_fill_viridis(name = Mass~(kg/km^2),
                     na.value = "white") +
  theme_map() +
  theme(plot.subtitle = element_text(face = "bold")) +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = extent(base.map)[3:4], xlim = extent(base.map)[1:2] * 1.02, expand = FALSE)

# Which cells corresponds to Fløjgaards data
cells <- cellFromXY(base.map, camilla %>% st_coordinates())


# Add realm information
realms <- tibble(realm = raster("data/wwf_terr_ecos_realm_raster.tif")[cells])
realms.names <- read_csv("data/wwf_realm_names.csv")
realms <- left_join(realms, realms.names, by = c("realm" = "wwf_realm"))
realms <- realms %>% mutate(name = ifelse(name %in% c("Nearctic", "Neotropic"), "Americas", name))
camilla$realm <- realms$name

# Make unq id's
camilla$cell <- cells
camilla$areaID <- paste(camilla$Name_Ecosystem_new, camilla$cell, sep = "_")

# Cut off low NPP based on WWF Deserts and Xeric shrub 
# Modis data are based on leaf area and solar energy input
# It is total NPP, and before _any_ consumption 
# (other than what has been subtracted for respiration)
npp <- raster("../large_datasets/MOD17A3/NPP_projected_resampled_scaled_gCarbon_m2_yr.tif") # [gC / m2 / yr]
# Cut extremely variable values (>95-quantile SD)
npp.cut <- raster("data/Q95_extreme_sd_npp1.tif")
npp[which(npp.cut[] == 1)] <- NA

# Cut off at 3rd quantile "most" ~ 200 gC/m2/yr
cut <- 200
npp[npp[] < cut] <- NA
remove.areas <- which(is.na(npp[]))

# Subset
camilla <- camilla %>% 
  filter(!cell %in% remove.areas)

# Load density data
density.samples <- read_csv("builds/sampled.density.distribution.csv", col_types = cols()) # [individuals / km2]

# Alignment sanity check 
stopifnot(all(names(density.samples) == df$Binomial.1.2))

# Transform to biomass [Mg / km2]
mass.samples <- sweep(density.samples, MARGIN = 2, df$Mass.g/1e3, `*`) # [kg / km2]

# cu maps
cu.maps <- read_rds("builds/current.maps.filtered.edge.lim.rds")

# Subset species
# To only above 5 kg
species.i <- which(df$Mass.g >= 5000 & df$Diet.Plant >= 80)

# Calculate total biomass per grid cell per map all-----------------------------

# Calculate a current mass map for every sample for every species
cu.mass.samples <- apply(mass.samples[, species.i], 1, function(.) . * cu.maps[species.i, camilla$cell]) 

# Sum total current mass for each sample
cu.mass <- sapply(cu.mass.samples, colSums) # [kg / km2]
rm(cu.mass.samples)
gc()

summary.stats <- function(x) {
  mean <- apply(x, 1, mean)
  geo.mean <- apply(x, 1, FUN = function(.) exp(mean(log(.))))
  geo.sd <- apply(x, 1, FUN = function(.) exp(sd(log(.))))
  q025 <- apply(x, 1, quantile, probs = .025)
  q975 <- apply(x, 1, quantile, probs = .975)
  
  res <- list(geo.mean = geo.mean, 
              geo.sd = geo.sd, 
              geo.low = geo.mean / geo.sd,
              geo.high = geo.mean * geo.sd,
              q025 = q025, 
              q975 = q975)

  return(res)
}

cu.summary <- summary.stats(cu.mass)

camilla$cu.geo.mass <- cu.summary$geo.mean
camilla$cu.geo.mass.low <- cu.summary$geo.low
camilla$cu.geo.mass.high <- cu.summary$geo.high

camilla %>% 
  arrange(desc(TotalHerbivoreBiomass_kgkm2)) %>% 
  dplyr::select(1:2, 4, 18:20) %>% 
  print(n=30)

camilla.summary = camilla %>%
  group_by(areaID) %>% 
  summarise(Mass = median(TotalHerbivoreBiomass_kgkm2),
            Mass.lw = min(TotalHerbivoreBiomass_kgkm2),
            Mass.hi = max(TotalHerbivoreBiomass_kgkm2),
            cu.geo.mass = cu.geo.mass[1],
            cu.geo.mass.low = cu.geo.mass.low[1],
            cu.geo.mass.high = cu.geo.mass.high[1],
            n = n(),
            realm = realm[1],
            Megafauna = max(Megafauna, na.rm = T)) %>%
  filter(n > 1)

cols <- colorspace::qualitative_hcl(palette = "Dark 3", n = 5)
breaks = c("Americas", "Afrotropic", "Palearctic", "Indomalaya", "Australasia")
c <- ggplot(camilla.summary, aes(cu.geo.mass, Mass, col = realm)) +
  geom_rect(xmin = 0, xmax = 10000, ymin = 0, ymax = 10000, fill = "#cccccc05", col = NA) +
  geom_abline(slope = 1, lty = "dashed") +
  geom_point(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(cu.geo.mass, TotalHerbivoreBiomass_kgkm2, col = realm)) +
  geom_point(data = camilla %>% filter(areaID %in% camilla.summary$areaID), aes(cu.geo.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
  geom_point(shape = 5) +
  theme_R() +
  theme(panel.grid = element_blank()) +
  coord_equal(xlim = c(0, 46000), ylim = c(0, 46000), expand = FALSE) +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
  geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi)) +
  geom_segment(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(x = cu.geo.mass.low, y = TotalHerbivoreBiomass_kgkm2, yend = ..y.., xend = cu.geo.mass.high), alpha =.3, lwd = .3) +
  geom_segment(aes(x = cu.geo.mass.low, y = Mass, yend = ..y.., xend = cu.geo.mass.high), alpha =.3, lwd = .3) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks)

c1 <- ggplot(camilla.summary, aes(cu.geo.mass, Mass, col = realm)) +
  geom_abline(slope = 1, lty = "dashed") +
  geom_point(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(cu.geo.mass, TotalHerbivoreBiomass_kgkm2, col = realm)) +
  geom_point(data = camilla %>% filter(areaID %in% camilla.summary$areaID), aes(cu.geo.mass, TotalHerbivoreBiomass_kgkm2), col = "grey") +
  geom_point(shape = 5) +
  theme_R() +
  theme(panel.grid = element_blank()) +
  coord_equal(xlim = c(0, 10000), ylim = c(0, 10000), expand = FALSE) +
  scale_x_continuous(minor_breaks = seq(0, 11000, 2500), breaks = seq(0, 11000, 5000)) +
  ylab(bquote("Total herbivore biomass (Fløjgaard et al.) [" ~ kg/km^2 ~ "]")) +
  xlab(bquote("Total herbivore biomass (This paper) [" ~ kg/km^2 ~ "]")) +
  geom_segment(aes(y = Mass.lw, xend = ..x.., yend = Mass.hi)) +
  geom_segment(data = camilla %>% filter(!areaID %in% camilla.summary$areaID), aes(x = cu.geo.mass.low, y = TotalHerbivoreBiomass_kgkm2, yend = ..y.., xend = cu.geo.mass.high), alpha =.3, lwd = .3) +
  geom_segment(aes(x = cu.geo.mass.low, y = Mass, yend = ..y.., xend = cu.geo.mass.high), alpha =.3, lwd = .3) +
  # annotate("text", x = 27911, y = 29000, label = expression("Masai Mara National Reserve Kenya" %->% ~ ""), hjust = 1) +
  # annotate("text", x = 1013, y = 11959, label = expression(" " %<-% "Oostvaardersplassen the Netherlands"), hjust = 0) +
  scale_color_manual(name = "Realm", values = cols, breaks = breaks)

p <- cowplot::plot_grid(c + theme(legend.position="none"),
                   c1 + theme(legend.position="none"),
                   align = 'none',
                   labels = c("a", "b"),
                   rel_widths = c(1, 1)
)

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend_b <- cowplot::get_legend(c + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- cowplot::plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .05))

ggsave("Output/Compare_fløjgaard.jpg", width = 183, height = 80, units = "mm", dpi = 600, scale = 1.45)


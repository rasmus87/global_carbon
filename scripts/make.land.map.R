# Make a map of what we consider land
# 28/07-2021 Rasmus Ø Pedersen

# Load libraries
library(sf) # Spatial data formats and handling
library(raster) # Raster handling
library(stars) # Much faster rasterization of poloygons (does lines too)
library(fasterize) # Even faster rasterization of poloygons (only midpoints)
library(tidyverse) # Datamanipulation etc.
library(ggthemes)
library(rnaturalearth)

# Load basemap
base.map <- raster("builds/base_map.tif")
stars.base.map <- st_as_stars(base.map)


# Load world map in small resolution (avoiding smaller islands and making 
# cleaner coast for our scale of analysis)
world.map <- ne_countries(scale = 110, returnclass = "sf") %>% 
  st_transform(st_crs(base.map)) %>% 
  st_union() %>% 
  st_sf()
# Cast as lines
world.map.lines <- st_cast(world.map, "MULTILINESTRING")

# Rasterize cell centers
range <- fasterize(st_sf(world.map), base.map, fun = "first", background = 0)
# Rasterize the edges: "ALL_TOUCHED=TRUE" makes it behave like raster::rasterize()
range.edges <- st_rasterize(world.map.lines, stars.base.map, options = "ALL_TOUCHED=TRUE")
range.edges <- as.vector(range.edges[[1]])
range.edges <- ifelse(is.na(range.edges), 0, 1)
# Combine centers and edges and set the rest to NA
range[] <- ifelse(range[] + range.edges, 1, 0)
range[range[] == 0] <- NA

# Write output
writeRaster(range, "builds/land.tif", overwrite = T)

# Test plot of the range
range.df <- as(range, "SpatialPixelsDataFrame") %>% as_tibble()
ggplot(range.df, aes(x = x, y = y, fill = as_factor(layer))) +
  geom_tile() +
  scale_fill_discrete(name = "Land",
                     na.value = "pink") +
  theme_map() +
  geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
  coord_sf(ylim = range(range.df$y))

 
# current.maps <- read_rds("builds/current.maps.filtered.rds")
# present.natural.maps <- read_rds("builds/present.natural.maps.filtered.rds")
# mammal.maps <- rbind(current.maps, present.natural.maps)
# 
# mammal.map <- base.map
# mammal.map[] <- colSums(mammal.maps)
# mammal.map[mammal.map[] > 0] <- 1
# mammal.map[mammal.map[] == 0] <- NA
# 
# x <- base.map
# x[which(mammal.map[] == 1 & is.na(range[]))] <- 1
# x.df <- as(x, "SpatialPixelsDataFrame") %>% as_tibble()
# 
# 
# ggplot(x.df, aes(x = x, y = y, fill = as_factor(base_map))) +
#   geom_tile() +
#   scale_fill_manual(name = "Missing land",
#                      values = "hot pink") +
#   theme_map() +
#   geom_sf(data = world.map, inherit.aes = F, col = "black", fill = "NA", lwd = .25) +
#   coord_sf(ylim = range(range.df$y))
# 
# plot(x)
# cell <- click() %>% cellFromXY(x, .)
# species <- which(mammal.maps[, cell] > 0) %>% names %>% unique
# species

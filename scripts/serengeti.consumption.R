library(tidyverse)
library(sp)
library(raster)

# McNaughton, S. J. (1985). Ecology of a Grazing Ecosystem: The Serengeti.
# Ecological Monographs, 55(3), 259–294. https://doi.org/10.2307/1942578
# Data from Fig 12
# Dry weight or just weight???
df <- read_csv("data/consumption_McNaughton1985.csv")

H.Mg_km2_yr <- df$H.g_m2_yr / 10^6 * 1000^2 # Mg / km2 / yr
summary(H.Mg_km2_yr) * 45/100

serengeti <- data.frame("long" = 34.955137, "lat" = -2.151406)
serengeti <- SpatialPoints(serengeti, proj4string=CRS("+init=epsg:4326"))
r <- raster("data/Continents for terrestrial mammals.tif")
serengeti <- spTransform(serengeti, projection(r))
cell <- cellFromXY(r, serengeti)

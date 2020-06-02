library(tidyverse)
library(sp)
library(raster)

df <- read_csv("data/consumption_McNaughton1985.csv")

summary(df$H.g_m2_yr)

H.Mg_km2_yr <- df$H.g_m2_yr / 10^6 * 1000^2
summary(H.Mg_km2_yr) * 45/100

serengeti <- data.frame("long" = 34.955137, "lat" = -2.151406)
serengeti <- SpatialPoints(serengeti, proj4string=CRS("+init=epsg:4326"))
r <- raster("data/Continents for terrestrial mammals.tif")
serengeti <- spTransform(serengeti, projection(r))
cell <- cellFromXY(r, serengeti)

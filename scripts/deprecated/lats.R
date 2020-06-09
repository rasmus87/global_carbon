library(raster)
library(rgdal)

r <- raster("./builds/base_map.tif")

column <- 1:(360*142) %% 360 == 1
r[column] <- 1

r.pts <- rasterToPoints(r, spatial = T)
plot(r.pts)

wgs84 <- crs("+init=epsg:4326")

r.pts <- spTransform(r.pts, wgs84)

row.lat <- r.pts@coords[,2]

write.csv(row.lat, "builds/row.lat.csv")

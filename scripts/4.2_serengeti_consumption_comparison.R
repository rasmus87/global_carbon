# Minor script to compare serengeti grazing to our study
# Run after 3.0_load.data.R
# 29/07-2021 Rasmus Ø Pedersen

# McNaughton, S. J. (1985). Ecology of a Grazing Ecosystem: The Serengeti.
# Ecological Monographs, 55(3), 259–294. https://doi.org/10.2307/1942578
# Data from Fig 12
# Assuming dry weight (it isn't explicitly stated)
df <- read_csv("data/consumption_McNaughton1985.csv")

# Unit conversion
H.Mg_km2_yr <- df$H.g_m2_yr / 10^6 * 1000^2 # Mg DW / km2 / yr

# Dry weight to Carbon:
dw_to_C <- 45/100
H.MgC_km2_yr <- df$H.g_m2_yr * dw_to_C # MgC / km2 / yr
summary(H.MgC_km2_yr)

serengeti <- data.frame("long" = 34.955137, "lat" = -2.151406)
serengeti <- SpatialPoints(serengeti, proj4string=CRS("+init=epsg:4326"))
r <- raster("builds/base_map.tif")
serengeti <- spTransform(serengeti, projection(r))
cell <- cellFromXY(r, serengeti)

paste0("Current: ", round(current.consumption.map[cell]), " MgC/km2/yr")
paste0("Current low .025: ", round(current.consumption.map.lw[cell]), " MgC/km2/yr")
paste0("Current low .925: ", round(current.consumption.map.hi[cell]), " MgC/km2/yr")
paste0("Present natural: ", round(present.natural.consumption.map[cell]), " MgC/km2/yr")

paste0("Current: ", round(current.npp.use[cell]), "% of NPP")
paste0("Present natural: ", round(present.natural.npp.use[cell]), "% of NPP")

paste0("NPP: ", round(npp[cell]), " MgC/km2/yr")

round(summary(H.MgC_km2_yr) / npp[cell] * 100)
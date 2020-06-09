# Load the filtered maps, and halve the range outline cells

library(raster)
base.map <- raster("builds/base_map.tif")

half_edge_range <- function(species.range) {
  species.map <- base.map
  species.range[species.range == 0] <- NA
  species.map[] <- species.range
  species.range.edges <- boundaries(species.map, 
                                    type = 'inner',
                                    classes = FALSE,
                                    directions = 4, 
                                    asNA = FALSE)
  species.range <- species.map[] - species.range.edges[]/2
  species.range[is.na(species.range[])] <- 0
  return(species.range)
}

gc()

load("builds/current.maps.filtered.RData")
load("builds/present.natural.maps.filtered.RData")

current.maps.edge.lim <- t(apply(current.maps, 1, half_edge_range))
save(current.maps.edge.lim, file = "builds/current.maps.filtered.edge.lim.RData")
present.natural.maps.edge.lim <- t(apply(present.natural.maps, 1, half_edge_range))
save(present.natural.maps.edge.lim, file = "builds/present.natural.maps.filtered.edge.lim.RData")

gc()

base.map[] <- colSums(present.natural.maps[])
plot(base.map, main = "present.natural.maps")

base.map[] <- colSums(present.natural.maps.edge.lim[])
plot(base.map, main = "present.natural.maps.edge.lim")

base.map[] <- colSums(current.maps[])
plot(base.map, main = "current.maps")

base.map[] <- colSums(current.maps.edge.lim[])
plot(base.map, main = "current.maps.edge.lim")
